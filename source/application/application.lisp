;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def constant +session-purge-time-interval+ 30)
(def constant +session-purge-request-interval+ (if *load-as-production?* 100 1))

(def (generic e) make-new-session (application))
(def (generic e) make-new-frame (application session))
(def (generic e) register-session (application session))
(def (generic e) register-frame (application session frame))
(def (generic e) delete-session (application session))
(def (generic e) purge-sessions (application)
  (:method :around (application)
    (with-thread-name " / PURGE-SESSIONS"
      (call-next-method))))

(def (special-variable e) *maximum-sessions-per-application-count* most-positive-fixnum
  "The default for the same slot in applications.")

(def (special-variable e) *default-session-timeout* (* 30 60)
  "The default for the same slot in applications.")

(def (special-variable e) *default-frame-timeout* *default-session-timeout*
  "The default for the same slot in applications.")

(def (special-variable e) *default-ajax-enabled* #t
  "The default for the same slot in applications.")

(def (special-variable e) *default-compile-time-debug-client-side* #f
  "The default for the same slot in applications.")

(def localization-loading-locale-loaded-listener wui-resource-loader/application :hu.dwim.wui "localization/application/" :log-discriminator "WUI")

(register-locale-loaded-listener 'wui-resource-loader/application)

(def (class* e) standard-application (application-with-home-package
                                      application-with-dojo-support)
  ()
  (:metaclass funcallable-standard-class))

(def (class* e) application (broker-with-path-prefix
                             request-counter-mixin
                             debug-context-mixin)
  ((requests-to-sessions-count 0 :type integer :export :accessor)
   (entry-points nil)
   (default-uri-scheme "http" :type string)
   (default-locale "en" :type string)
   (supported-locales '("en"))
   (default-timezone local-time:*default-timezone* :type local-time::timezone)
   (session-class :type standard-class)
   (session-timeout *default-session-timeout*)
   (frame-timeout *default-frame-timeout* :type integer)
   (sessions-last-purged-at (get-monotonic-time) :type number)
   (maximum-sessions-count *maximum-sessions-per-application-count* :type integer)
   (session-id->session (make-hash-table :test 'equal) :type hash-table :export :accessor)
   (frame-root-component-factory 'default-frame-root-component-factory :type (or symbol function) :documentation "A funcallable with (body-content &key &allow-other-keys) lambda-list, which is invoked to make the toplevel components for new frames. By default calls MAKE-FRAME-COMPONENT-WITH-CONTENT.")
   (administrator-email-address nil :type string)
   (lock)
   (running-in-test-mode #f :type boolean :accessor running-in-test-mode? :export :accessor)
   (compile-time-debug-client-side *default-compile-time-debug-client-side* :type boolean :accessor compile-time-debug-client-side? :export :accessor)
   (ajax-enabled *default-ajax-enabled* :type boolean :accessor ajax-enabled?))
  (:metaclass funcallable-standard-class)
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
                            (return-from broker-path-to-broker (nreverse path))))))

(def function total-web-session-count (server)
  (bind ((sum 0))
    (map-broker-tree server (lambda (path)
                              (bind ((el (first path)))
                                (when (typep el 'application)
                                  (incf sum (hash-table-count (session-id->session-of el)))))))
    sum))

(def function map-broker-tree (root visitor)
  (macrolet ((visit ()
               `(funcall visitor path))
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
  (setf (lock-of -self-) (make-recursive-lock (format nil "Application lock for ~A" path-prefix)))
  (setf (session-class-of -self-)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (session-class -self-))
                       :name (format-symbol :hu.dwim.wui "~A-SESSION-FOR-~A"
                                            (class-name (class-of -self-))
                                            (string-upcase path-prefix))))
  (set-funcallable-instance-function
   -self- (lambda (request)
          (application-handler -self- request))))

(def (generic e) call-in-application-environment (application session thunk)
  (:documentation "Everything related to an application goes through this method, so it can be used to set up wrappers like WITH-TRANSACTION. The SESSION argument may or may not be a valid session.")
  (:method (application session thunk)
    (app.dribble "CALL-IN-APPLICATION-ENVIRONMENT is calling the thunk")
    (funcall thunk)))

(def (generic e) call-in-post-action-environment (application session frame thunk)
  (:documentation "This call wraps entry points and rendering, but does not wrap actions. The SESSION argument may or may not be a valid session.")
  (:method (application session frame thunk)
    (app.dribble "CALL-IN-POST-ACTION-ENVIRONMENT is calling the thunk")
    (funcall thunk)))

(def (generic e) call-action (application session frame action)
  (:method (application session frame (action function))
    (funcall action))
  (:method :around (application session frame action)
    (bind ((*inside-user-code* #t))
      (call-next-method))))

(def (with-macro* eo) with-session-logic (&key ensure-session (requires-valid-session #t) (lock-session #t))
  (assert (and (boundp '*application*)
               *application*
               (boundp '*session*)
               (boundp '*frame*))
          () "May not use WITH-SESSION-LOGIC outside the dynamic extent of an application")
  (bind ((application *application*)
         (session nil)
         (session-instance nil)
         (session-cookie-exists? #f)
         (invalidity-reason nil)
         (new-session? #f))
    (app.debug "WITH-SESSION-LOGIC speaking, request is delayed-content? ~A, ajax-aware? ~A" *delayed-content-request* *ajax-aware-request*)
    (with-lock-held-on-application (application)
      (setf (values session session-cookie-exists? invalidity-reason session-instance)
            (find-session-from-request application))
      (when (and (not session)
                 (eq invalidity-reason :nonexistent)
                 ensure-session)
        (setf session (make-new-session application))
        (register-session application session)
        (setf new-session? #t))
      ;; FIXME locking the session should happen while having the lock to the application
      )
    (abort-request-unless-still-valid)
    (setf *session* session)
    (if session
        (bind ((local-time:*default-timezone* (client-timezone-of session)))
          (incf (requests-to-sessions-count-of application))
          (restart-case
              (bind ((response (if lock-session
                                   (progn
                                     (app.debug "WITH-SESSION-LOGIC is locking session ~A as requested" session)
                                     ;; TODO check if locking would hang, throw error if so
                                     (with-lock-held-on-session (session)
                                       (when (is-request-still-valid?)
                                         (call-in-application-environment application session #'-body-))))
                                   (progn
                                     (app.debug "WITH-SESSION-LOGIC is NOT locking session ~A, it wasn't requested" session)
                                     (call-in-application-environment application session #'-body-)))))
                (when (and new-session?
                           response)
                  (decorate-application-response application response))
                response)
            (delete-current-session ()
              :report (lambda (stream)
                        (format stream "Delete session ~A and rety handling the request" session))
              (mark-expired session)
              (invoke-retry-handling-request-restart))))
        (if (or requires-valid-session
                (not (eq invalidity-reason :nonexistent)))
            (bind ((response (handle-request-to-invalid-session application session invalidity-reason)))
              (decorate-application-response application response)
              response)
            (call-in-application-environment application nil #'-body-)))))

(def (with-macro* eo) with-frame-logic (&key (requires-valid-frame #t) (ensure-frame #f))
  (assert (and *application* *session* (boundp '*frame*)) () "May not use WITH-FRAME-LOGIC without a proper session in the environment")
  (app.debug "WITH-FRAME-LOGIC speaking, requires-valid-frame ~A, ensure-frame ~A" requires-valid-frame ensure-frame)
  (bind ((application *application*)
         (session *session*)
         ((:values frame frame-id-parameter-received? invalidity-reason frame-instance) (when session
                                                                                          (find-frame-for-request session))))
    (setf *frame* frame)
    (app.debug "WITH-FRAME-LOGIC looked up frame ~A from session ~A" frame session)
    (if frame
        (-body-)
        (cond
          ((and requires-valid-frame
                (or (not session)
                    frame-id-parameter-received?))
           (handle-request-to-invalid-frame application session frame-instance invalidity-reason))
          ((and ensure-frame
                *session*)
           ;; set up a new frame and fall through to the entry points to set up to their favour
           (setf frame (make-new-frame application session))
           (setf (id-of frame) (insert-with-new-random-hash-table-key (frame-id->frame-of session)
                                                                      frame +frame-id-length+))
           (register-frame application session frame)
           (setf *frame* frame)
           (-body-))
          (t
           (-body-))))))

(def (with-macro* eo) with-action-logic ()
  (assert (and *application* *session* *frame*) () "May not use WITH-ACTION-LOGIC without a proper application/session/frame dynamic environment")
  (bind ((application *application*)
         (session *session*)
         (frame *frame*))
    (assert-session-lock-held session)
    ;; TODO here? find its place...
    (notify-activity session)
    (labels ((convert-to-primitive-response* (response)
               (app.debug "Calling CONVERT-TO-PRIMITIVE-RESPONSE for ~A while still inside the WITH-LOCK-HELD-ON-SESSION's and WITH-ACTION-LOGIC's dynamic scope" response)
               (decorate-application-response *application* response)
               (convert-to-primitive-response response))
             (call-body ()
               (values (convert-to-primitive-response* (-body-)))))
      (declare (inline call-body))
      (if frame
          (progn
            (restart-case
                (progn
                  (setf *frame* frame)
                  (notify-activity frame)
                  (process-client-state-sinks frame (query-parameters-of *request*))
                  (bind ((action (find-action-from-request frame))
                         (incoming-frame-index (parameter-value +frame-index-parameter-name+))
                         (current-frame-index (frame-index-of frame))
                         (next-frame-index (next-frame-index-of frame)))
                    (unless (stringp current-frame-index)
                      (setf current-frame-index (integer-to-string current-frame-index)))
                    (unless (stringp next-frame-index)
                      (setf next-frame-index (integer-to-string next-frame-index)))
                    (app.debug "Incoming frame-index is ~S, current is ~S, next is ~S, action is ~A" incoming-frame-index current-frame-index next-frame-index action)
                    (cond
                      ((and action
                            incoming-frame-index)
                       (bind ((original-frame-index nil))
                         (unwind-protect-case ()
                             (if (equal incoming-frame-index next-frame-index)
                                 (progn
                                   (app.dribble "Found an action and frame is in sync...")
                                   (unless *delayed-content-request*
                                     (setf original-frame-index (step-to-next-frame-index frame)))
                                   (app.dribble "Calling action...")
                                   (bind ((response (call-action application session frame action)))
                                     (when (typep response 'response)
                                       (return-from with-action-logic
                                         (convert-to-primitive-response* response)))))
                                 (return-from with-action-logic
                                   (convert-to-primitive-response* (handle-request-to-invalid-frame application session frame :out-of-sync))))
                           (:abort
                            ;; TODO the problem at hand is this: when the app specific error handler is called the stack is not yet unwinded
                            ;; so this REVERT-STEP-TO-NEXT-FRAME-INDEX is not yet called, therefore the page it renders will point to an invalid
                            ;; frame index after this unwind block is executed.
                            ;; but on the other hand without this uwp, the "retry rendering this request" restart is broken...
                            ;; we chose the lesser badness here and don't do the revert, so break the restart instead of the user visible error page
                            #+nil
                            (when original-frame-index
                              (revert-step-to-next-frame-index frame original-frame-index))))))
                      (incoming-frame-index
                       (unless (equal incoming-frame-index current-frame-index)
                         (return-from with-action-logic
                           (convert-to-primitive-response*
                            (handle-request-to-invalid-frame application session frame :out-of-sync)))))
                      ;; at the time the frame is first registered, there's no frame index param in the url, so just fall through here and
                      ;; end up at the entry points.
                      )
                    (app.dribble "Action logic fell through, proceeding to the thunk...")))
              (delete-current-frame ()
                :report (lambda (stream)
                          (format stream "Delete frame ~A" frame))
                (mark-expired frame)
                (invoke-retry-handling-request-restart)))
            (call-body))
          (if *ajax-aware-request*
              ;; we can't find a valid frame but received an ajax aware request. for now treat this as an
              ;; error until a valid use-case requires something else...
              (frame-not-found-error)
              (call-body))))))

(def type session-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated))

(def (generic e) handle-request-to-invalid-session (application session invalidity-reason)
  (:method :before ((application application) session invalidity-reason)
    (check-type invalidity-reason session-invalidity-reason)
    (check-type session (or null session)))
  (:method ((application application) session invalidity-reason)
    (app.debug "Default HANDLE-REQUEST-TO-INVALID-SESSION is sending a redirect response to ~A" application)
    (make-redirect-response-for-current-application)))

(def type frame-invalidity-reason ()
  `(member :nonexistent :timed-out :invalidated :out-of-sync))

(def (generic e) handle-request-to-invalid-frame (application session frame invalidity-reason)
  (:method :before ((application application) session frame invalidity-reason)
    (check-type invalidity-reason frame-invalidity-reason)
    (check-type frame (or null frame)))
  (:method ((application application) session frame invalidity-reason)
    (app.dribble "Default HANDLE-REQUEST-TO-INVALID-FRAME speeking, invalidity-reason is ~S" invalidity-reason)
    (if (eq invalidity-reason :out-of-sync)
        (bind ((refresh-href   (print-uri-to-string (make-uri-for-current-frame)))
               (new-frame-href (print-uri-to-string (make-uri-for-new-frame)))
               (args (list refresh-href new-frame-href)))
          (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a frame out of sync response")
          (emit-simple-html-document-http-response (:status +http-not-acceptable+
                                                    :headers '#.+disallow-response-caching-header-values+)
            (apply-localization-function 'render-frame-out-of-sync-error args))
          (make-do-nothing-response))
        (progn
          (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a redirect response to ~A" application)
          (make-redirect-response-for-current-application)))))

(def (generic e) handle-request-to-invalid-action (application session frame action invalidity-reason)
  (:method ((application application) session frame action invalidity-reason)
    (declare (type (member :nonexistent :timed-out :invalidated) invalidity-reason)
             (type (or null action) action))
    (app.debug "Default HANDLE-REQUEST-TO-INVALID-ACTION is sending a redirect response to ~A" application)
    (make-redirect-response-for-current-application)))

(def (function e) invoke-delete-current-frame-restart ()
  (invoke-restart (find-restart 'delete-current-frame)))

(def (function e) invoke-delete-current-session-restart ()
  (invoke-restart (find-restart 'delete-current-session)))

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

(def (function o) application-handler (application request)
  (assert (not (boundp '*rendering-phase-reached*)))
  (assert (not (boundp '*inside-user-code*)))
  (bind ((*application* application)
         (*debug-client-side* (compile-time-debug-client-side? application))
         (*fallback-locale-for-functional-localizations* (default-locale-of application))
         (*brokers* (cons application *brokers*))
         ;; bind *session* and *frame* here, so that WITH-SESSION/FRAME/ACTION-LOGIC and entry-points can freely setf it
         (*session* nil)
         (*frame* nil)
         (*rendering-phase-reached* #f)
         (*inside-user-code* #f)
         ;; the request counter is not critical, so just ignore locking...
         (request-counter (incf (processed-request-count-of application))))
    (when (and (zerop (mod request-counter +session-purge-request-interval+)) ; get time less often
               (> (- (get-monotonic-time)
                     (sessions-last-purged-at-of application))
                  +session-purge-time-interval+))
      (purge-sessions application))
    (handle-request application request)))

(def method handle-request ((application application) request)
  (assert (eq *application* application))
  (assert (eq (first *brokers*) application))
  (bind (((:values matches? *application-relative-path*) (request-uri-matches-path-prefix? application request)))
    (when matches?
      (with-locale (default-locale-of application)
        (bind ((*ajax-aware-request* (ajax-aware-request?))
               (*delayed-content-request* (or *ajax-aware-request*
                                              (delayed-content-request?)))
               (local-time:*default-timezone* (default-timezone-of application)))
          (app.debug "~A matched with *application-relative-path* ~S, querying entry-points for response" application *application-relative-path*)
          (bind ((response (query-brokers-for-response request (entry-points-of application))))
            (when response
              (unwind-protect
                   (progn
                     (app.debug "Calling SEND-RESPONSE for ~A while still inside the dynamic extent of the HANDLE-REQUEST method of application" response)
                     (send-response response))
                (close-response response))
              (make-do-nothing-response))))))))

(def (generic e) session-class (application)
  (:documentation "Returns a list of the session mixin classes.

Custom implementations should look something like this:
\(def method session-class list \(\(app your-application))
  'your-session-mixin)")
  (:method-combination list))

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

(def method register-frame ((application application) (session session) (frame frame))
  (assert-session-lock-held session)
  (assert (null (session-of frame)) () "The frame ~A is already registered to a session" frame)
  (assert (or (not (boundp '*frame*))
              (null *frame*)
              (eq *frame* frame)))
  (bind ((frame-id->frame (frame-id->frame-of session)))
    ;; TODO purge frames
    (bind ((frame-id (insert-with-new-random-hash-table-key frame-id->frame frame +frame-id-length+)))
      (setf (id-of frame) frame-id)
      (setf (session-of frame) session)
      (app.dribble "Registered frame with id ~S" (id-of frame))
      frame)))

(def method register-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (null (application-of session)) () "The session ~A is already registered to an application" session)
  (assert (or (not (boundp '*session*))
              (null *session*)
              (eq *session* session)))
  (bind ((session-id->session (session-id->session-of application)))
    (when (> (hash-table-count session-id->session)
             (maximum-sessions-count-of application))
      (too-many-sessions application))
    (bind ((session-id (insert-with-new-random-hash-table-key session-id->session session +session-id-length+)))
      (setf (id-of session) session-id)
      (setf (application-of session) application)
      (app.dribble "Registered session with id ~S" (id-of session))
      session)))

(def method delete-session ((application application) (session session))
  (assert-application-lock-held application)
  (assert (eq (application-of session) application))
  (app.dribble "Deleting session ~A" session)
  (bind ((session-id->session (session-id->session-of application)))
    (remhash (id-of session) session-id->session))
  (values))

(def (method o) purge-sessions ((application application))
  (app.dribble "Purging the sessions of ~S" application)
  ;; this method should be called while not holding any session or application lock
  (assert (not (is-lock-held? (lock-of application))) () "You must NOT have a lock on the application when calling PURGE-SESSIONS (or on any of its sessions)!")
  (setf (sessions-last-purged-at-of application) (get-monotonic-time))
  (let ((deleted-sessions (list))
        (live-sessions (list)))
    (with-lock-held-on-application (application)
      (iter (for (session-id session) :in-hashtable (session-id->session-of application))
            (if (is-session-alive? session)
                (push session live-sessions)
                (block deleting-session
                  (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                                  (app.error "Could not delete expired/invalid session ~A of application ~A, got error ~A" session application error))
                                                (lambda (&key &allow-other-keys)
                                                  (return-from deleting-session)))
                    (delete-session application session)
                    (push session deleted-sessions))))))
    (dolist (session deleted-sessions)
      (block noifying-session
        (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                        (app.error "Error happened while notifying session ~A of application ~A about its exiration, got error ~A" session application error))
                                      (lambda (&key &allow-other-keys)
                                        (return-from noifying-session)))
          (with-lock-held-on-session (session)
            (notify-session-expiration application session)))))
    (dolist (session live-sessions)
      (block purging-session-frames
        (with-layered-error-handlers ((lambda (error &key &allow-other-keys)
                                        (app.error "Error happened while purging frames of ~A of application ~A. Got error ~A" session application error))
                                      (lambda (&key &allow-other-keys)
                                        (return-from purging-session-frames)))
          (with-lock-held-on-session (session)
            (purge-frames application session)))))
    (values)))

(def (class* e) application-with-home-package (application)
  ((home-package *package* :type package))
  (:metaclass funcallable-standard-class))

(def method call-in-application-environment :around ((application application-with-home-package) session thunk)
  (let ((*package* (home-package-of application)))
    (call-next-method)))

;;;;;;
;;; Handle conditions in AJAX requests

(def function maybe-invoke-debugger/application (condition &key (context (or (and (boundp '*session*)
                                                                                  *session*)
                                                                             (and (boundp '*application*)
                                                                                  *application*)
                                                                             (and (boundp '*brokers*)
                                                                                  (first *brokers*)))))
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

(def class* locked-session-response-mixin (response)
  ())

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

(def (function e) make-application-relative-uri (relative-path)
  (append-path-to-uri (make-uri-for-application *application*) relative-path))

(def (function e) make-uri-for-application (application &optional relative-path)
  "Does not assume an application context and only uses *session* & co. when available."
  (bind ((uri (clone-request-uri)))
    (clear-uri-query-parameters uri)
    (decorate-uri uri application)
    (when relative-path
      (append-path-to-uri uri relative-path))
    uri))

(def (function e) make-uri-for-current-application (&optional relative-path)
  "Expects a valid *session* environment."
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

(def (function e) make-static-content-uri-for-current-application (&optional relative-path)
  (make-uri :path (string+ (path-prefix-of *application*) relative-path)))

(def (function e) mark-all-sessions-expired (application)
  (with-lock-held-on-application (application)
    (iter (for (key session) :in-hashtable (session-id->session-of application))
          (mark-expired session))))

(def (class* e) application-with-dojo-support (application)
  ((dojo-skin-name nil)
   (dojo-file-name nil)
   (dojo-directory-name nil))
  (:metaclass funcallable-standard-class))

(def method call-in-application-environment :around ((application application-with-dojo-support) session thunk)
  (bind ((*dojo-skin-name* (or (dojo-skin-name-of application)
                               *dojo-skin-name*))
         (*dojo-file-name* (or (dojo-file-name-of application)
                               *dojo-file-name*))
         (*dojo-directory-name* (or (dojo-directory-name-of application)
                                    *dojo-directory-name*)))
    (call-next-method)))
