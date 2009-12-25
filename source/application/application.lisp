;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def localization-loader-callback wui-resource-loader/application :hu.dwim.wui "localization/application/" :log-discriminator "WUI")

(register-locale-loaded-listener 'wui-resource-loader/application)

(def (class* e) standard-application (application-with-home-package
                                      application-with-dojo-support)
  ())

(def (class* e) application (broker-with-path-prefix
                             request-counter-mixin
                             debug-context-mixin)
  ((requests-to-sessions-count 0 :type integer :export :accessor)
   (entry-points nil)
   (default-uri-scheme "http" :type string)
   (default-locale "en" :type string)
   (supported-locales '("en") :type sequence)
   (default-timezone local-time:*default-timezone* :type local-time::timezone)
   (session-class nil :type (or null standard-class))
   (session-timeout *default-session-timeout* :type number)
   (frame-timeout *default-frame-timeout* :type integer)
   (sessions-last-purged-at (get-monotonic-time) :type number)
   (maximum-sessions-count *maximum-sessions-per-application-count* :type integer)
   (session-id->session (make-hash-table :test 'equal) :type hash-table :export :accessor)
   (frame-root-component-factory 'default-frame-root-component-factory :type (or symbol function) :documentation "A funcallable with (body-content &key &allow-other-keys) lambda-list, which is invoked to make the toplevel components for new frames. By default calls MAKE-FRAME-COMPONENT-WITH-CONTENT.")
   (administrator-email-address nil :type (or null string))
   (lock)
   (running-in-test-mode #f :type boolean :accessor running-in-test-mode? :export :accessor)
   (compile-time-debug-client-side *default-compile-time-debug-client-side* :type boolean :accessor compile-time-debug-client-side? :export :accessor)
   (ajax-enabled *default-ajax-enabled* :type boolean :accessor ajax-enabled?))
  (:default-initargs :path-prefix "/"))

(def function default-frame-root-component-factory (content)
  (make-frame-component-with-content *application* *session* *frame* content))

(def function call-frame-root-component-factory (content)
  (funcall (frame-root-component-factory-of *application*) content))

(def method debug-on-error? ((application application) error)
  (cond
    ((slot-boundp application 'debug-on-error)
     (slot-value application 'debug-on-error))
    ;; TODO: there's some anomaly here compared to sessions/apps: sessions are owned by an application, but apps don't have a server slot. resolve or ignore? are apps exclusively owned by servers?
    ((and (boundp '*server*)
          *server*)
     (debug-on-error? *server* error))
    (t (call-next-method))))

(def method compile-time-debug-client-side? :around ((self application))
  (if (slot-boundp self 'compile-time-debug-client-side)
      (call-next-method)
      (or (running-in-test-mode? self)
          (not *load-as-production?*))))

(def (function e) human-readable-broker-path (server application)
  (bind ((path (broker-path-to-broker server application)))
    (assert (member application path))
    (apply #'string+
           (iter (for el :in path)
                 (etypecase el
                   (application (collect (path-prefix-of el)))
                   (server nil))))))

(def function broker-path-to-broker (root broker)
  (map-broker-tree root (lambda (path)
                          (when (eq (first path) broker)
                            (return-from broker-path-to-broker (nreverse path))))
                   :visit-type :path))

(def function total-web-session-count (server)
  (bind ((sum 0))
    (map-broker-tree server (lambda (el)
                              (when (typep el 'application)
                                (incf sum (hash-table-count (session-id->session-of el))))))
    sum))

(def function map-broker-tree (root visitor &key
                                    (filter (constantly #t))
                                    (visit-type :leaf))
  (check-type visit-type (member :path :leaf))
  (macrolet ((visit ()
               `(bind ((el (ecase visit-type
                             (:path path)
                             (:leaf (first path)))))
                  (when (funcall filter el)
                    (funcall visitor el))))
             (recurse (children)
               `(map nil (lambda (child)
                           (%recurse (cons child path)))
                     ,children)))
    (labels ((%recurse (path)
               (bind ((el (first path)))
                 (etypecase el
                   ;; TODO handle delegate-broker when implemented
                   (broker-based-server (visit)
                                        (recurse (brokers-of el)))
                   (application (visit)
                                (recurse (entry-points-of el)))
                   (entry-point (visit))
                   (broker nil)
                   (null nil)))))
      (%recurse (list root))))
  (values))

(def (function i) assert-application-lock-held (application)
  (assert (is-lock-held? (lock-of application)) () "You must have a lock on the application here"))

(def (with-macro* e) with-lock-held-on-application (application)
  (debug-only*
    (iter (for (nil session) :in-hashtable (session-id->session-of application))
          (assert (not (is-lock-held? (lock-of session))) ()
                  "You are trying to lock the application ~A while one of its session ~A is already locked by you"
                  application session)))
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-application for app ~S in thread ~S" application (current-thread))
        (with-recursive-lock-held ((lock-of application))
          (-body-)))
    (threads.dribble "Leaving with-lock-held-on-application for app ~S in thread ~S" application (current-thread))))

(def (constructor o) (application path-prefix)
  (assert path-prefix)
  (setf (lock-of -self-) (make-recursive-lock (format nil "Application lock for ~A" path-prefix))))

(def method session-class-of :around ((self application))
  (or (call-next-method)
      (setf (session-class-of self)
            (aprog1
                (make-instance 'standard-class
                               :direct-superclasses (mapcar #'find-class (session-class self))
                               :name (symbolicate '#:session-class-for/
                                                  (class-name (class-of self))
                                                  "/"
                                                  (path-prefix-of self)))
              (app.debug "Instantiated session class ~A for application ~A" it self)))))

(def method session-class list ((application application))
  'session)

(def method make-new-session ((application application))
  (make-instance (session-class-of application)
                 :time-to-live (session-timeout-of application)))

(def method make-new-frame ((application application) (session session))
  (assert session)
  (app.debug "Creating new frame for session ~A of app ~A" session application)
  (assert-session-lock-held session)
  (make-instance 'frame
                 :time-to-live (frame-timeout-of application)))

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
        (bind ((response (query-brokers-for-response request (entry-points-of application) :otherwise nil)))
          (when response
            (unwind-protect
                 (progn
                   (app.debug "Calling SEND-RESPONSE for ~A while still inside the dynamic extent of the PRODUCE-RESPONSE method of application" response)
                   (send-response response))
              (close-response response))
            ;; TODO why not unwinding from here instead of make-do-nothing-response?
            (make-do-nothing-response)))))))

;;;;;;
;;; application-with-home-package

(def (class* e) application-with-home-package (application)
  ((home-package *package* :type package)))

(def method call-in-application-environment :around ((application application-with-home-package) session thunk)
  (bind ((*package* (home-package-of application)))
    (call-next-method)))

;;;;;;
;;; error handling in AJAX requests

(def function maybe-invoke-debugger/application (condition &key (context (or (and (boundp '*session*)
                                                                                  *session*)
                                                                             (and (boundp '*application*)
                                                                                  *application*)
                                                                             (and (boundp '*broker-stack*)
                                                                                  (first *broker-stack*)))))
  (maybe-invoke-debugger condition :context context))

(def method handle-toplevel-error :around ((application application) condition)
  (if (and (boundp '*ajax-aware-request*)
           *ajax-aware-request*)
      (progn
        (maybe-invoke-debugger/application condition)
        (emit-http-response ((+header/status+       +http-not-acceptable+
                              +header/content-type+ +xml-mime-type+))
          <ajax-response
           <error-message ,#"error-message-for-ajax-requests">
           <result "failure">>))
      (call-next-method)))

;;;;;;
;;; Application specific responses

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

;;;;;;
;;; Utils

;; TODO: not used, maybe waiting to be deleted?
#+nil
(def (function e) make-redirect-response-with-frame-index-decorated (&optional (frame *frame*))
  (bind ((uri (clone-request-uri)))
    (assert (and frame (not (null (id-of frame)))))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame))
    (make-redirect-response uri)))

(def (function e) make-redirect-response-with-frame-id-decorated (&optional (frame *frame*))
  (bind ((uri (clone-request-uri)))
    (assert (and frame (not (null (id-of frame)))))
    (setf (uri-query-parameter-value uri +frame-id-parameter-name+) (id-of frame))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame))
    (make-redirect-response uri)))

(def (function e) make-uri-for-current-application (&optional relative-path)
  (assert *application*)
  (bind ((uri (clone-request-uri)))
    (clear-uri-query-parameters uri)
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

#+nil ; TODO err... i don't get it. rename or delme.
(def (function e) make-static-content-uri-for-current-application (&optional relative-path)
  (make-uri :path (string+ (path-prefix-of *application*) relative-path)))
