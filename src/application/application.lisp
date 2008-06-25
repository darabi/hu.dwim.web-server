;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def constant +session-purge-time-interval+ 1)
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

(def (special-variable e) *session-timeout* (* 30 60)
  "The default for the same slot in applications.")

(def (special-variable e) *frame-timeout* *session-timeout*
  "The default for the same slot in applications.")

(def (special-variable e) *application*)

(def (function e) make-application (&rest args &key (path-prefix "/") &allow-other-keys)
  (apply #'make-instance 'application :path-prefix path-prefix args))

(def class* application (broker-with-path-prefix)
  ((entry-points nil)
   (default-uri-scheme "http")
   (default-locale "en")
   (session-class)
   (session-timeout *session-timeout*)
   (frame-timeout *frame-timeout*)
   (processed-request-count 0)
   (sessions-last-purged-at (get-monotonic-time))
   (maximum-number-of-sessions *maximum-number-of-sessions-per-application*)
   (session-id->session (make-hash-table :test 'equal))
   (lock))
  (:metaclass funcallable-standard-class))

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

(def (generic e) call-as-handler-in-session (application session thunk)
  (:method :before ((application application) (session session) thunk)
    (assert-session-lock-held session)
    (notify-activity session))
  (:method ((application application) (session session) thunk)
    (bind ((frame (find-frame-from-request session)))
      (flet ((send-response-early (response)
               (app.debug "Calling SEND-RESPONSE for ~A while still inside the WITH-LOCK-HELD-ON-SESSION's dynamic scope" response)
               (decorate-application-response application response)
               (send-response response)
               (make-do-nothing-response)))
        (if frame
            (restart-case
                (progn
                  (setf *frame* frame)
                  (notify-activity frame)
                  (process-client-state-sinks frame (query-parameters-of *request*))
                  (bind ((action (find-action-from-request frame))
                         (incoming-frame-index (parameter-value +frame-index-parameter-name+)))
                    (app.debug "Incoming frame-index is ~S, current is ~S, next is ~S, action is ~A" incoming-frame-index (frame-index-of frame) (next-frame-index-of frame) action)
                    (when (and (stringp incoming-frame-index)
                               (zerop (length incoming-frame-index)))
                      (setf incoming-frame-index nil))
                    (cond
                      ((and action
                            incoming-frame-index)
                       (if (equal incoming-frame-index (next-frame-index-of frame))
                           (progn
                             (unless (request-for-delayed-content?)
                               (step-to-next-frame-index frame))
                             (bind ((response (funcall action)))
                               (when (typep response 'response)
                                 (return-from call-as-handler-in-session
                                   (if (typep response 'locked-session-response-mixin)
                                       (send-response-early response)
                                       response)))))
                           (frame-out-of-sync-error frame)))
                      (incoming-frame-index
                       (unless (equal incoming-frame-index (frame-index-of frame))
                         (frame-out-of-sync-error frame)))
                      (t
                       (return-from call-as-handler-in-session (make-redirect-response-with-frame-id-decorated frame))))))
              (delete-current-frame ()
                :report (lambda (stream)
                          (format stream "Delete frame ~A" frame))
                (mark-expired frame)
                (invoke-retry-handling-request-restart)))
            (progn
              (setf frame (make-new-frame application session))
              (setf (id-of frame) (insert-with-new-random-hash-table-key (frame-id->frame-of session)
                                                                         frame +frame-id-length+))
              (register-frame application session frame)
              (setf *frame* frame)))
       (bind ((response (funcall thunk)))
         (if (and response
                  (typep response 'locked-session-response-mixin))
             (send-response-early response)
             response))))))

(def (with-macro* eo) with-session/frame/action-logic (&optional _)
  (declare (ignore _)) ; to force an extra args param for the with-... macro
  (assert (and (boundp '*application*)
               *application*
               (boundp '*session*)
               (boundp '*frame*))
          () "May not use WITH-SESSION/FRAME/ACTION-LOGIC outside the dynamic extent of an application")
  (bind ((application *application*)
         (session (with-lock-held-on-application (application)
                    (find-session-from-request application)
                    ;; FIXME locking the session should happen inside the with-lock-held-on-application block
                    )))
    (setf *session* session)
    (if session
        (restart-case
            (with-lock-held-on-session (session)
              (call-as-handler-in-session application session (lambda ()
                                                                (-body-))))
          (delete-current-session ()
            :report (lambda (stream)
                      (format stream "Delete session ~A" session))
            (mark-expired session)
            (invoke-retry-handling-request-restart)))
        (bind ((*frame* nil))
          (-body-)))))

(def (function e) invoke-delete-current-frame-restart ()
  (invoke-restart (find-restart 'delete-current-frame)))

(def (function e) invoke-delete-current-session-restart ()
  (invoke-restart (find-restart 'delete-current-session)))

(def (function e) decorate-application-response (application response)
  (when (and response
             *session*)
    (bind ((request-uri (uri-of *request*)))
      (app.debug "Decorating response ~A with the session cookie for session ~S" response *session*)
      (add-cookie (make-cookie
                   +session-cookie-name+
                   (aif *session*
                        (id-of it)
                        "")
                   :comment "WUI session id"
                   :domain (concatenate-string "." (host-of request-uri))
                   :path (path-prefix-of application))
                  response)))
  response)

(def (function o) application-handler (application request)
  (bind ((*application* application)
         ;; the request counter is not critical, so just ignore locking...
         (request-counter (incf (processed-request-count-of application))))
    (when (and (zerop (mod request-counter +session-purge-request-interval+)) ; get time less often
               (> (- (get-monotonic-time)
                     (sessions-last-purged-at-of application))
                  +session-purge-time-interval+))
      (purge-sessions application))
    ;; bind *session* and *frame* here, so that WITH-SESSION/FRAME/ACTION-LOGIC and entry-points can setf it
    (bind ((*session* nil)
           (*frame* nil)
           (response (handle-request application request)))
      (decorate-application-response application response)
      response)))

(def method handle-request ((application application) request)
  (bind (((:values matches? relative-path) (matches-request-uri-path-prefix? application request)))
    (when matches?
      (app.debug "~A matched with relative-path ~S, querying entry-points for response" application relative-path)
      (query-entry-points-for-response application request relative-path))))

(def (function o) query-entry-points-for-response (application initial-request relative-path)
  (bind ((*brokers* (cons application *brokers*))
         (results (multiple-value-list
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
  (app.dribble "Deleting session with id ~S" (id-of session))
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
            (if (is-timed-out? session)
                (handler-bind ((serious-condition
                                (lambda (error)
                                  (app.warn "Could not delete expired session ~A of application ~A, got error ~A" session application error)
                                  (log-error-with-backtrace error))))
                  (delete-session application session)
                  (push session deleted-sessions))
                (push session live-sessions))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; app specific responses

(def (layered-function e) render (component)
  (:method :around (component)
    (call-in-rendering-environment component #'call-next-method)))

(def (definer e) render (&body forms)
  (bind ((layer (when (member (first forms) '(:in-layer :in))
                  (pop forms)
                  (pop forms)))
         (qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms)))
    `(def layered-method render ,@(when layer `(:in ,layer)) ,@(when qualifier (list qualifier)) ((-self- ,type))
       ,@forms)))

(def (generic e) call-in-rendering-environment (component thunk)
  (:method (component thunk)
    (funcall thunk)))

(def (definer e) call-in-rendering-environment (&body forms)
  (bind ((qualifier (when (or (keywordp (first forms))
                              (member (first forms) '(and or progn append nconc)))
                      (pop forms)))
         (type (pop forms))
         (unused (gensym)))
    `(def method call-in-rendering-environment ,@(when qualifier (list qualifier)) ((-self- ,type) ,unused)
          ,@forms)))

;; TODO: move?
(def function collect-path-to-root-component (component)
  (iter (for ancestor-component :initially component :then (parent-component-of ancestor-component))
        (while ancestor-component)
        (collect ancestor-component)))

(def function render-with-restored-rendering-environment (component)
  (bind ((path (nreverse (cdr (collect-path-to-root-component component)))))
    (labels ((%render-with-restored-rendering-environment (remaining-path)
               (if remaining-path
                   (call-in-rendering-environment (car remaining-path)
                                                  (lambda ()
                                                    (%render-with-restored-rendering-environment (cdr remaining-path))))
                   (render component))))
      (%render-with-restored-rendering-environment path))))

(def function ajax-aware-render (component ajax-aware-client?)
  (if ajax-aware-client?
      (bind ((dirty-components (collect-covering-remote-identity-components-for-dirty-descendant-components component)))
        <ajax-response
         ,@(with-collapsed-js-scripts
            <dom-replacements
             ,@(mappend [ensure-list (render-with-restored-rendering-environment !1)] dirty-components)>)>)
      (render component)))

(def (function e) render-to-string (component &key (ajax-aware-client #f))
  (bind ((*request* (make-instance 'request :uri (parse-uri "")))
         (*application* (make-instance 'application :path-prefix ""))
         (*session* (make-instance 'session))
         (*frame* (make-instance 'frame :session *session*)))
    (setf (id-of *session*) "1234567890")
    (with-lock-held-on-session (*session*)
      (octets-to-string
       (with-output-to-sequence (buffer-stream :external-format :utf-8
                                               :initial-buffer-size 256)
         (emit-into-html-stream buffer-stream
           (ajax-aware-render component ajax-aware-client)))
       :encoding :utf-8))))

(def class* locked-session-response-mixin (response)
  ())

;;;;;
;;; Component rendering response

(def special-variable *debug-component-hierarchy* #f)

(def class* component-rendering-response (locked-session-response-mixin)
  ((layer-context (current-layer-context))
   (application)
   (session)
   (frame)
   (component)))

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

(def (constant :test 'string=) +ajax-aware-client-parameter-name+ "_j")

(def function ajax-aware-client? (&optional (request *request*))
  (bind ((value (request-parameter-value request +ajax-aware-client-parameter-name+)))
    (and value
         (not (string= value "")))))

(def method send-response ((self component-rendering-response))
  (disallow-response-caching self)
  (bind ((*frame* (frame-of self))
         (*session* (session-of self))
         (*application* (application-of self))
         (*debug-component-hierarchy* (if *frame* (debug-component-hierarchy-p *frame*) *debug-component-hierarchy*))
         (body (with-output-to-sequence (buffer-stream :external-format (external-format-of self)
                                                       :initial-buffer-size 256)
                 (when (and *frame*
                            (not (request-for-delayed-content?)))
                   (clrhash (action-id->action-of *frame*))
                   (clrhash (client-state-sink-id->client-state-sink-of *frame*)))
                 (emit-into-html-stream buffer-stream
                   (funcall-with-layer-context (layer-context-of self)
                                               (lambda () (ajax-aware-render (component-of self) (ajax-aware-client?)))))))
         (headers (with-output-to-sequence (header-stream :element-type '(unsigned-byte 8)
                                                          :initial-buffer-size 128)
                    (setf (header-value self +header/content-length+) (princ-to-string (length body)))
                    (send-http-headers (headers-of self) (cookies-of self) :stream header-stream))))
    ;; TODO use multiplexing when writing to the network stream, including the headers
    (write-sequence headers (network-stream-of *request*))
    (write-sequence body (network-stream-of *request*)))
  (values))


;;;;;;;;;
;;; utils

(def (function e) make-redirect-response-with-frame-id-decorated (&optional (frame *frame*))
  (bind ((uri (clone-uri (uri-of *request*))))
    (assert (and frame (not (null (id-of frame)))))
    (setf (uri-query-parameter-value uri +frame-index-parameter-name+) (frame-index-of frame))
    (make-redirect-response uri)))

(def (function e) make-uri-for-application (application &optional relative-path)
  "Does not assume an application context and only use *session* & co. when available."
  (bind ((uri (clone-uri (uri-of *request*))))
    (clear-uri-query-parameters uri)
    (decorate-uri uri application)
    (when relative-path
      (append-path-to-uri uri relative-path))
    uri))

(def (function e) make-uri-for-current-application (&optional relative-path)
  "Expects a valid *session* environment."
  (bind ((uri (clone-uri (uri-of *request*))))
    (clear-uri-query-parameters uri)
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (when relative-path
      (append-path-to-uri uri relative-path))
    uri))

(def (function e) make-redirect-response-for-current-application (&optional relative-path)
  (make-redirect-response (make-uri-for-current-application relative-path)))

(def (function e) make-static-content-uri-for-current-application (&optional relative-path)
  (make-uri :path (concatenate-string (path-prefix-of *application*) relative-path)))

(def (function e) mark-all-sessions-expired (application)
  (with-lock-held-on-application (application)
    (iter (for (key session) :in-hashtable (session-id->session-of application))
          (mark-expired session))))
