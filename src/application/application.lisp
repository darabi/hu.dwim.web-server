;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def constant +session-purge-time-interval+ 30)
(def constant +session-purge-request-interval+ (if *load-as-production-p* 100 1))

(def (generic e) make-new-session (application))
(def (generic e) make-new-frame (application session))
(def (generic e) register-session (application session))
(def (generic e) register-frame (application session frame))
(def (generic e) delete-session (application session))
(def (generic e) purge-sessions (application)
  (:method :around (application)
    (with-thread-name " / PURGE-SESSIONS"
      (call-next-method))))

(def (special-variable e) *maximum-number-of-sessions-per-application* most-positive-fixnum
  "The default for the same slot in applications.")

(def (special-variable e) *default-session-timeout* (* 30 60)
  "The default for the same slot in applications.")

(def (special-variable e) *default-frame-timeout* *default-session-timeout*
  "The default for the same slot in applications.")

(def (function e) make-application (&rest args &key (path-prefix "/") &allow-other-keys)
  (apply #'make-instance 'application :path-prefix path-prefix args))

(def (class* e) application (broker-with-path-prefix)
  ((entry-points nil)
   (default-uri-scheme "http")
   (default-locale "en")
   (default-timezone local-time:*default-timezone*)
   (session-class)
   (session-timeout *default-session-timeout*)
   (frame-timeout *default-frame-timeout*)
   (processed-request-count 0)
   (sessions-last-purged-at (get-monotonic-time))
   (maximum-number-of-sessions *maximum-number-of-sessions-per-application*)
   (session-id->session (make-hash-table :test 'equal))
   (admin-email-address nil)
   (lock)
   (running-in-test-mode #f :type boolean :export :accessor)
   (compile-time-debug-client-side :type boolean :accessor compile-time-debug-client-side? :export :accessor))
  (:metaclass funcallable-standard-class))

(def method compile-time-debug-client-side? :around ((self application))
  (if (slot-boundp self 'compile-time-debug-client-side)
      (call-next-method)
      (or (running-in-test-mode-p self)
          (not *load-as-production-p*))))

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
    (funcall thunk)))

(def (generic e) call-action (application session frame action)
  (:method (application session frame (action funcallable-standard-object))
    (funcall action)))

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
         (invalidity-reason nil))
    (app.debug "Request is delayed-content? ~A, ajax-aware? ~A" *delayed-content-request* *ajax-aware-request*)
    (with-lock-held-on-application (application)
      (setf (values session session-cookie-exists? invalidity-reason session-instance)
            (find-session-from-request application))
      (when (and (not session)
                 ensure-session)
        (setf session (make-new-session application))
        (register-session application session))
      ;; FIXME locking the session should happen while having the lock to the application
      )
    (abort-request-unless-still-valid)
    (setf *session* session)
    (if session
        (bind ((local-time:*default-timezone* (client-timezone-of session)))
          (restart-case
              (if lock-session
                  ;; TODO check if locking would hang, throw error if so
                  (with-lock-held-on-session (session)
                    (when (is-request-still-valid?)
                      (call-in-application-environment application session #'-body-)))
                  (call-in-application-environment application session #'-body-))
            (delete-current-session ()
              :report (lambda (stream)
                        (format stream "Delete session ~A and rety handling the request" session))
              (mark-expired session)
              (invoke-retry-handling-request-restart))))
        (if requires-valid-session
            (bind ((response (handle-request-to-invalid-session application session invalidity-reason)))
              (decorate-application-response application response)
              response)
            (-body-)))))

(def (with-macro* eo) with-frame-logic (&key (requires-valid-frame #t) (ensure-frame #f))
  (assert (and *application* *session* (boundp '*frame*)) () "May not use WITH-FRAME-LOGIC without a proper session in the environment")
  (bind ((application *application*)
         (session *session*)
         ((:values frame frame-id-parameter-received? invalidity-reason frame-instance) (when session
                                                                                          (find-frame-from-request session))))
    (setf *frame* frame)
    (app.dribble "WITH-FRAME-LOGIC looked up frame ~A from session ~A" frame session)
    (if frame
        (-body-)
        (cond
          ((and requires-valid-frame
                (or (not session)
                    frame-id-parameter-received?))
           (handle-request-to-invalid-frame application session frame-instance invalidity-reason))
          ((and ensure-frame
                *session*)
           ;; set up a new frame and fall through to the entry points to set up a root-component to their favour
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
    (flet ((maybe-send-response-early (response)
             (if (and response
                      (typep response 'locked-session-response-mixin))
                 (progn
                   (app.debug "Calling SEND-RESPONSE for ~A while still inside the WITH-LOCK-HELD-ON-SESSION's and WITH-ACTION-LOGIC's dynamic scope" response)
                   (decorate-application-response application response)
                   (send-response response)
                   (make-do-nothing-response))
                 response)))
      (bind ((response
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
                                                 (maybe-send-response-early response)))))
                                         (handle-request-to-invalid-frame application session frame :out-of-sync))
                                   (:abort
                                    (when original-frame-index
                                      (revert-step-to-next-frame-index frame original-frame-index))))))
                              (incoming-frame-index
                               (unless (equal incoming-frame-index current-frame-index)
                                 (return-from with-action-logic
                                   (maybe-send-response-early
                                    (handle-request-to-invalid-frame application session frame :out-of-sync)))))
                              #+nil ; TODO think about this. when the frame is registeres, there's no frame index param, but it's still a valid request
                              (t
                               (frame-index-missing-error frame)))
                            (app.dribble "Action logic fell through, proceeding to the thunk...")))
                      (delete-current-frame ()
                        :report (lambda (stream)
                                  (format stream "Delete frame ~A" frame))
                        (mark-expired frame)
                        (invoke-retry-handling-request-restart)))
                    (-body-))
                  (if *ajax-aware-request*
                      ;; we can't find a valid frame but received an ajax aware request. for now treat this as an
                      ;; error until a valid use-case requires something else...
                      (frame-not-found-error)
                      (-body-)))))
        (maybe-send-response-early response)))))

(def (generic e) handle-request-to-invalid-session (application session invalidity-reason)
  (:method ((application application) session invalidity-reason)
    (declare (type (member :nonexistent :timed-out :invalidated) invalidity-reason)
             (type (or null session) session))
    (app.debug "Default HANDLE-REQUEST-TO-INVALID-SESSION is sending a redirect response to ~A" application)
    (make-redirect-response-for-current-application)))

(def (generic e) handle-request-to-invalid-frame (application session frame invalidity-reason)
  (:method ((application application) session frame invalidity-reason)
    (declare (type (member :nonexistent :timed-out :invalidated :out-of-sync) invalidity-reason)
             (type (or null frame) frame))
    (app.dribble "Default HANDLE-REQUEST-TO-INVALID-FRAME speeking")
    (if (eq invalidity-reason :out-of-sync)
        (bind ((refresh-href   (print-uri-to-string (make-uri-for-current-frame)))
               (new-frame-href (print-uri-to-string (make-uri-for-new-frame)))
               (args (list refresh-href new-frame-href)))
          (app.debug "Default HANDLE-REQUEST-TO-INVALID-FRAME is sending a frame out of sync response")
          (emit-simple-html-document-http-response (:status +http-not-acceptable+
                                                    :headers '#.+disallow-response-caching-header-values+)
            (lookup-resource 'render-frame-out-of-sync-error
                             :arguments args
                             :otherwise (lambda ()
                                          (apply 'render-frame-out-of-sync-error/english args))))
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
                   :domain (concatenate-string "." (host-of request-uri))
                   :path (path-prefix-of application))
                  response)))
  response)

(def (function o) application-handler (application request)
  (bind ((*application* application)
         (*brokers* (cons application *brokers*))
         ;; bind *session* and *frame* here, so that WITH-SESSION/FRAME/ACTION-LOGIC and entry-points can freely setf it
         (*session* nil)
         (*frame* nil)
         ;; the request counter is not critical, so just ignore locking...
         (request-counter (incf (processed-request-count-of application))))
    (when (and (zerop (mod request-counter +session-purge-request-interval+)) ; get time less often
               (> (- (get-monotonic-time)
                     (sessions-last-purged-at-of application))
                  +session-purge-time-interval+))
      (purge-sessions application))
    (handle-request application request)))

(def method handle-request ((application application) request)
  (bind (((:values matches? relative-path) (request-uri-matches-path-prefix? application request)))
    (when matches?
      (with-locale (default-locale-of application)
        (bind ((*ajax-aware-request* (ajax-aware-request?))
               (*delayed-content-request* (or *ajax-aware-request*
                                              (delayed-content-request?)))
               (local-time:*default-timezone* (default-timezone-of application)))
          (app.debug "~A matched with relative-path ~S, querying entry-points for response" application relative-path)
          (bind ((response (query-entry-points-for-response application request relative-path)))
            (when response
              (app.debug "Calling SEND-RESPONSE for ~A while still inside the dynamic extent of the HANDLE-REQUEST method of application" response)
              (send-response response)
              (make-do-nothing-response))))))))

(def (function o) query-entry-points-for-response (application initial-request relative-path)
  (assert (eq *application* application))
  (assert (eq (first *brokers*) application))
  (bind ((results (multiple-value-list
                   (iterate-brokers-for-response (lambda (broker request)
                                                   (if (typep broker 'entry-point)
                                                       (funcall broker request application relative-path)
                                                       (funcall broker request)))
                                                 initial-request
                                                 (entry-points-of application)
                                                 (entry-points-of application)
                                                 0))))
    (if (first results)
        (values-list results)
        (make-no-handler-response))))

(def (generic e) session-class (application)
  (:documentation "Returns a list of the session mixin classes.

Custom implementations should look something like this:
\(defmethod session-class list \(\(app your-application))
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
             (maximum-number-of-sessions-of application))
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
                (handler-bind ((serious-condition
                                (lambda (error)
                                  (app.warn "Could not delete expired/invalid session ~A of application ~A, got error ~A" session application error)
                                  (log-error-with-backtrace error))))
                  (delete-session application session)
                  (push session deleted-sessions)))))
    (dolist (session deleted-sessions)
      (handler-bind ((serious-condition
                      (lambda (error)
                        (app.warn "Error happened while notifying session ~A of application ~A about its exiration, got error ~A" session application error)
                        (log-error-with-backtrace error))))
        (with-lock-held-on-session (session)
          (notify-session-expiration application session))))
    (dolist (session live-sessions)
      (handler-bind ((serious-condition
                      (lambda (error)
                        (app.warn "Error happened while purging frames of ~A of application ~A. Got error ~A" session application error)
                        (log-error-with-backtrace error))))
        (with-lock-held-on-session (session)
          (purge-frames application session))))
    (values)))

(def (class* e) application-with-home-package (application)
  ((home-package))
  (:metaclass funcallable-standard-class))

(def method call-in-application-environment :around ((application application-with-home-package) session thunk)
  (let ((*package* (home-package-of application)))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; app specific responses

(def function render* (component)
  (bind ((*rendering-in-progress* #t))
    (render component)))

(def (layered-function e) render (component))

(def (layered-function e) render-csv (component))

(def (layered-function e) render-pdf (component))

(def (layered-function e) render-odf (component))

(def (definer e) render (&body forms)
  (render-like-definer 'render forms))

(def (definer e) render-csv (&body forms)
  (render-like-definer 'render-csv forms))

(def (definer e) render-pdf (&body forms)
  (render-like-definer 'render-pdf forms))

(def (definer e) render-odf (&body forms)
  (render-like-definer 'render-odf forms))

(def function render-like-definer (name forms)
  (bind ((layer (when (member (first forms) '(:in-layer :in))
                  (pop forms)
                  (pop forms)))
         (qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms)))
    `(def layered-method ,name ,@(when layer `(:in ,layer)) ,@(when qualifier (list qualifier)) ((-self- ,type))
          ,@forms)))

(def (generic e) call-in-component-environment (component thunk)
  (:method (component thunk)
    (funcall thunk)))

(def macro with-component-environment (component &body forms)
  `(call-in-component-environment ,component (lambda () ,@forms)))

(def (definer e) call-in-component-environment (&body forms)
  (bind ((qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms))
         (unused (gensym)))
    `(def method call-in-component-environment ,@(when qualifier (list qualifier)) ((-self- ,type) ,unused)
        ,@forms)))

;; TODO: move?
(def function collect-path-to-root-component (component)
  (iter (for ancestor-component :initially component :then (parent-component-of ancestor-component))
        (while ancestor-component)
        (collect ancestor-component)))

(def function call-with-restored-component-environment (component thunk)
  (bind ((path (nreverse (collect-path-to-root-component component))))
    (labels ((%call-with-restored-component-environment (remaining-path)
               (if remaining-path
                   (with-component-environment (car remaining-path)
                     (%call-with-restored-component-environment (cdr remaining-path)))
                   (funcall thunk))))
      (%call-with-restored-component-environment path))))

(def (macro e) with-restored-component-environment (component &body forms)
  `(call-with-restored-component-environment ,component (lambda () ,@forms)))

(def function ajax-aware-render (component)
  (app.debug "Inside AJAX-AWARE-RENDER; is this an ajax-aware-request? ~A" *ajax-aware-request*)
  (if *ajax-aware-request*
      (bind ((dirty-components (collect-covering-remote-identity-components-for-dirty-descendant-components component)))
        (setf (header-value *response* +header/content-type+) +xml-mime-type+)
        ;; FF does not like proper xml prologue, probably the other browsers even more so...
        ;; (emit-xml-prologue +encoding+)
        <ajax-response
         ,@(with-collapsed-js-scripts
             (with-dojo-widget-collector
               <dom-replacements (:xmlns #.+xhtml-namespace-uri+)
                 ,(foreach (lambda (dirty-component)
                             (with-restored-component-environment (parent-component-of dirty-component)
                               (render* dirty-component)))
                           dirty-components)>))
         <result "success">>)
      (render* component)))

(def (function e) render-to-string (component &key ajax-aware)
  (bind ((*request* (make-instance 'request :uri (parse-uri "")))
         (*application* (make-instance 'application :path-prefix ""))
         (*session* (make-instance 'session))
         (*frame* (make-instance 'frame :session *session*))
         (*ajax-aware-request* ajax-aware))
    (setf (id-of *session*) "1234567890")
    (with-lock-held-on-session (*session*)
      (octets-to-string
       (with-output-to-sequence (buffer-stream :external-format :utf-8
                                               :initial-buffer-size 256)
         (emit-into-html-stream buffer-stream
           (ajax-aware-render component)))
       :encoding :utf-8))))

(def class* locked-session-response-mixin (response)
  ())

;;;;;
;;; Component rendering response

(def special-variable *debug-component-hierarchy* #f)

(def class* component-rendering-response (locked-session-response-mixin)
  ((unique-counter 0)
   (application)
   (session)
   (frame)
   (component)))

;; TODO switch default content-type to +xhtml-mime-type+ (search for other uses, too)
(def (function e) make-component-rendering-response (component &key (application *application*) (session *session*) (frame *frame*)
                                                               (encoding +encoding+) (content-type (content-type-for +html-mime-type+ encoding)))
  (aprog1
      (make-instance 'component-rendering-response
                     :component component
                     :application application
                     :session session
                     :frame frame)
    (setf (header-value it +header/content-type+) content-type)))

(def (function e) make-root-component-rendering-response (frame &key (encoding +encoding+) (content-type (content-type-for +html-mime-type+ encoding)))
  (bind ((session (session-of frame))
         (application (application-of session)))
    (make-component-rendering-response (root-component-of frame)
                                       :application application
                                       :session session
                                       :frame frame
                                       :encoding encoding
                                       :content-type content-type)))

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

(def method send-response ((self component-rendering-response))
  (disallow-response-caching self)
  (bind ((*frame* (frame-of self))
         (*session* (session-of self))
         (*application* (application-of self))
         (*debug-component-hierarchy* (if *frame* (debug-component-hierarchy-p *frame*) *debug-component-hierarchy*))
         (*ajax-aware-request* (ajax-aware-request?))
         (*delayed-content-request* (or *ajax-aware-request*
                                        (delayed-content-request?)))
         (body (with-output-to-sequence (buffer-stream :external-format (external-format-of self)
                                                       :initial-buffer-size 256)
                 (when (and *frame*
                            (not *delayed-content-request*))
                   (app.debug "This is not a delayed content request, clearing the action and client-state-sink hashtables of ~A" *frame*)
                   (clrhash (action-id->action-of *frame*))
                   (clrhash (client-state-sink-id->client-state-sink-of *frame*)))
                 (emit-into-html-stream buffer-stream
                   (bind ((start-time (get-monotonic-time)))
                     (multiple-value-prog1
                         (ajax-aware-render (component-of self))
                       (app.info "Rendering done in ~,3f secs" (- (get-monotonic-time) start-time)))))))
         (headers (with-output-to-sequence (header-stream :element-type '(unsigned-byte 8)
                                                          :initial-buffer-size 256)
                    (setf (header-value self +header/content-length+) (integer-to-string (length body)))
                    (send-http-headers (headers-of self) (cookies-of self) :stream header-stream))))
    ;; TODO use multiplexing when writing to the network stream, including the headers
    (app.debug "Sending component rendering response of ~A bytes" (length body))
    (write-sequence headers (network-stream-of *request*))
    (write-sequence body (network-stream-of *request*)))
  (values))


;;;;;;;;;
;;; utils

#+nil ; not used, maybe waiting to be deleted?
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
  (make-uri :path (concatenate-string (path-prefix-of *application*) relative-path)))

(def (function e) mark-all-sessions-expired (application)
  (with-lock-held-on-application (application)
    (iter (for (key session) :in-hashtable (session-id->session-of application))
          (mark-expired session))))
