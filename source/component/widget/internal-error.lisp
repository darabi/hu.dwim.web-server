;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; internal-error-message/widget

;; TODO inherit from panel/widget instead of these (currently title is not rendered)
;; TODO this is broken as is: if RENDERING-PHASE-REACHED is true then it means that the frame contains
;; something that fails to render, potentially the first topmost component. it means that currently the browser
;; window is useless until the frame-id is deleted from the url by hand...
;; TODO is this missing a render method? its command bar is not rendered...
(def (component e) internal-error-message/widget (component-messages/widget
                                                  content/widget
                                                  title/mixin
                                                  id/mixin)
  ((rendering-phase-reached :type boolean))
  (:default-initargs :title #"error.internal-server-error.title" :id "internal-error"))

(def layered-method make-command-bar-commands ((self internal-error-message/widget) class prototype value)
  (list* (make-instance 'command/widget
                        :content (icon navigate-back)
                        :action (if (rendering-phase-reached? self)
                                    (make-uri-for-new-frame)
                                    (make-uri-for-current-frame)))
         (call-next-method)))

(def method handle-toplevel-error ((application application) (error serious-condition))
  (when (and (not *inside-user-code*)
             *session*)
    ;; oops, this error comes from inside WUI or at least not from an action or from render. let's try to invalidate the session if there's any...
    (server.warn "Invalidating session ~A because an error came from inside WUI while processing a request coming to it" *session*)
    (mark-session-invalid *session*)
    (setf *session* nil))
  (if (and *session*
           *inside-user-code*)
      (progn
        (app.debug "HANDLE-TOPLEVEL-ERROR for application with a valid *SESSION* and *INSIDE-USER-CODE*")
        (app.error (build-backtrace-string error))
        (bind ((request-uri (raw-uri-of *request*)))
          (if (or (not *response*)
                  (not (headers-are-sent-p *response*)))
              (progn
                (server.info "Sending an internal server error page for request ~S coming to application ~A" request-uri application)
                (bind ((*response* nil) ; leave alone the original value, and keep an assert from firing
                       (rendering-phase-reached *rendering-phase-reached*)
                       (response (make-component-rendering-response
                                  (call-frame-root-component-factory
                                   (aprog1
                                       (make-instance 'internal-error-message/widget
                                                      :rendering-phase-reached rendering-phase-reached
                                                      :content (inline-render-component/widget ()
                                                                 (apply-localization-function 'render-application-internal-error-page
                                                                                              (list :administrator-email-address (administrator-email-address-of application)))))
                                     (add-component-error-message it #"error.internal-server-error.message"))))))
                  (unwind-protect
                       (send-response response)
                    (close-response response))))
              (server.info "Internal server error for request ~S to application ~A and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri application)))
        (abort-server-request error))
      (call-next-method)))
