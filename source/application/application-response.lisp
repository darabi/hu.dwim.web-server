;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (function e) decorate-application-response (application response)
  (when response
    (bind ((request-uri (uri-of *request*)))
      (app.debug "Decorating response ~A with the session cookie for session ~S" response *session*)
      (add-cookie (make-cookie
                   +session-cookie-name+
                   (aif *session*
                        (id-of it)
                        "")
                   :max-age (unless *session*
                              0)
                   :comment "WUI session id"
                   :domain (string+ "." (host-of request-uri))
                   :path (path-prefix-of application))
                  response)))
  response)

(def function ajax-aware-request? (&optional (request *request*))
  "Did the client js side code notify us that it's ready to receive ajax answers?"
  (bind ((value (request-parameter-value request +ajax-aware-parameter-name+)))
    (and value
         (etypecase value
           (cons (some [not (string= !1 "")] value))
           (string (not (string= value "")))))))

(def function delayed-content-request? (&optional (request *request*))
  "A delayed content request is supposed to render stuff to the same frame that was delayed at the main request (i.e. tooltips)."
  (bind ((value (request-parameter-value request +delayed-content-parameter-name+)))
    (and value
         (etypecase value
           (cons (some [not (string= !1 "")] value))
           (string (not (string= value "")))))))

(def method produce-response ((application application) request)
  (assert (not (boundp '*rendering-phase-reached*)))
  (assert (not (boundp '*inside-user-code*)))
  (assert (eq (first *broker-stack*) application))
  (bind ((*application* application)
         (request-number (processed-request-counter/increment application)))
    (when (and (zerop (mod request-number +session-purge/check-at-request-interval+)) ; to call GET-MONOTONIC-TIME less often
               (> (- (get-monotonic-time)
                     (sessions-last-purged-at-of application))
                  +session-purge/time-interval+))
      (purge-sessions application))
    (with-locale (default-locale-of application)
      (bind ((local-time:*default-timezone* (default-timezone-of application))
             ;; bind *session* and *frame* here, so that WITH-SESSION/FRAME/ACTION-LOGIC and entry-points can freely setf it
             (*session* nil)
             (*frame* nil)
             (*application-relative-path* (remaining-path-of-request-uri request))
             (*fallback-locale-for-functional-localizations* (default-locale-of application))
             (*rendering-phase-reached* #f)
             (*inside-user-code* #f)
             (*debug-client-side* (compile-time-debug-client-side? application))
             (*ajax-aware-request* (ajax-aware-request?))
             (*delayed-content-request* (or *ajax-aware-request*
                                            (delayed-content-request?))))
        (app.debug "~A matched with *application-relative-path* ~S, querying entry-points for response" application *application-relative-path*)
        (assert (or (not *ajax-aware-request*)
                    *delayed-content-request*))
        (if *delayed-content-request*
            (progn
              (app.debug "This is a *DELAYED-CONTENT-REQUEST*, handling appropriately")
              (with-session-logic (:requires-valid-session #t)
                (with-frame-logic (:requires-valid-frame #t)
                  (with-action-logic (:requires-valid-action #t)
                    (assert nil () "Execution must not get inside WITH-ACTION-LOGIC for ajax requests. How did this happen?")))))
            (bind ((response (query-brokers-for-response request (entry-points-of application) :otherwise nil)))
              (when response
                (unwind-protect
                     (progn
                       (app.debug "Calling SEND-RESPONSE for ~A while still inside the dynamic extent of the PRODUCE-RESPONSE method of application" response)
                       (send-response response))
                  (close-response response))
                ;; TODO why not unwinding from here instead of make-do-nothing-response?
                (make-do-nothing-response))))))))

;;;;;;
;;; Error handling in AJAX requests

;; TODO search all usages of this, and factor out what makes sense
(def macro emit-error-response-for-ajax-aware-client (() &body body)
  `(emit-http-response ((+header/status+       +http-ok+
                         +header/content-type+ +xml-mime-type+))
     <ajax-response
       <result "success">
       ,@,@body>))

(def method handle-toplevel-error :around ((application application) condition)
  (if (and (boundp '*ajax-aware-request*)
           *ajax-aware-request*)
      (progn
        (maybe-invoke-debugger/application condition)
        (emit-error-response-for-ajax-aware-client ()
          <script `js-inline(wui.io.inform-user-about-ajax-error #"error.internal-server-error.message")>))
      (call-next-method)))

;;;;;;
;;; Utils

(def (function e) make-redirect-response-with-frame-id-decorated (&optional (frame *frame*))
  (bind ((uri (clone-request-uri)))
    (assert (and frame (not (null (id-of frame)))))
    (setf (uri-query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame))
    (make-redirect-response uri)))

(def (function e) make-uri-for-current-application (&optional relative-path)
  (assert *application*)
  (bind ((uri (clone-request-uri)))
    (uri/delete-all-query-parameters uri)
    (decorate-uri uri *application*)
    (when *session*
      (decorate-uri uri *session*))
    (when *frame*
      (decorate-uri uri *frame*))
    (when relative-path
      (append-path-to-uri uri relative-path))
    uri))

(def function make-uri-for-current-frame ()
  (bind ((uri (clone-request-uri)))
    (setf (uri-query-parameter-value uri +action-id-parameter-name+) nil)
    (decorate-uri uri *frame*)
    uri))

(def function make-uri-for-new-frame ()
  (bind ((uri (clone-request-uri)))
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (setf (uri-query-parameter-value uri +frame-id-parameter-name+) nil)
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) nil)
    uri))

(def (function e) make-redirect-response-for-current-application (&optional relative-path)
  (make-redirect-response (make-uri-for-current-application relative-path)))