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

(def (function e) make-application (&key (path-prefix ""))
  (make-instance 'application :path-prefix path-prefix))

(def class* application (broker-with-path-prefix)
  ((entry-points nil)
   (default-uri-scheme "http")
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

(def with-macro with-lock-held-on-application (application)
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
  (setf (lock-of self) (make-recursive-lock (format nil "Application lock for ~A" path-prefix)))
  (setf (session-class-of self)
        (make-instance 'standard-class
                       :direct-superclasses (mapcar #'find-class (session-class self))
                       :name (format-symbol :hu.dwim.wui "~A-SESSION-FOR-~A"
                                            (class-name (class-of self))
                                            (string-upcase path-prefix))))
  (set-funcallable-instance-function
   self (lambda (request)
          (application-handler self request))))

(def (with-macro eo) with-looked-up-and-locked-session ()
  (assert (and (boundp '*application*)
               *application*
               (boundp '*session*)
               (boundp '*frame*))
          () "May not use WITH-LOOKED-UP-AND-LOCKED-SESSION outside the dynamic extent of an application")
  (bind ((application *application*)
         (session (with-lock-held-on-application (application)
                    (find-session-from-request application)
                    ;; FIXME locking the session should happen inside the with-lock-held-on-application block
                    )))
    (setf *session* session)
    (if session
        (block looking-up-frame
          (with-lock-held-on-session (session)
            (notify-activity session)
            (bind ((frame (find-frame-from-request session)))
              (if frame
                  (progn
                    (notify-activity frame)
                    ;; TODO only incf if an action was identified
                    (incf (frame-index-of frame))
                    (bind ((expected-frame-index (1- (frame-index-of frame)))
                           (incoming-frame-index (parse-integer (parameter-value +frame-index-parameter-name+) :junk-allowed #t)))
                      (if incoming-frame-index
                          (when (not (= incoming-frame-index expected-frame-index))
                            (frame-out-of-sync-error frame))
                          (return-from looking-up-frame (make-redirect-response-with-frame-id-decorated frame)))))
                  (progn
                    (setf frame (make-new-frame application session))
                    (setf (id-of frame) (insert-with-new-random-hash-table-key (frame-id->frame-of session)
                                                                               frame +frame-id-length+))))
              (setf *frame* frame)
              (-body-))))
        (bind ((*frame* nil))
          (-body-)))))

(def (function o) application-handler (application request)
  (bind ((*application* application)
         ;; the request counter is not critical, so just ignore locking...
         (request-counter (incf (processed-request-count-of application))))
    (when (and (zerop (mod request-counter +session-purge-request-interval+)) ; get time less often
               (> (- (get-monotonic-time)
                     (sessions-last-purged-at-of application))
                  +session-purge-time-interval+))
      (purge-sessions application))
    ;; bind *session* and *frame* here, so that WITH-LOOKED-UP-AND-LOCKED-SESSION and entry-points can setf it
    (bind ((*session* nil)
           (*frame* nil)
           (response (handle-request application request)))
      (when (and response
                 *session*)
        (bind ((request-uri (uri-of request)))
          (app.debug "Decorating response with the session cookie for session ~S" *session*)
          (add-cookie (make-cookie
                       +session-id-parameter-name+
                       (aif *session*
                            (id-of it)
                            "")
                       :comment "WUI session id"
                       :domain (concatenate-string "." (host-of request-uri))
                       :path (path-prefix-of application))
                      response)))
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
        +no-handler-response+)))

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
                 :time-to-live (frame-timeout-of application)
                 :frame-index (incf (current-frame-index-of session))))

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

(def class* root-component-rendering-response (response)
  ((frame)))

(def function make-root-component-rendering-response (frame)
  (make-instance 'root-component-rendering-response :frame frame))

(def method send-response ((self root-component-rendering-response))
  (bind ((*frame* (frame-of self))
         (*session* (session-of *frame*))
         (*application* (application-of *session*))
         (body (with-output-to-sequence (buffer-stream :external-format (external-format-of self)
                                                       :initial-buffer-size 256)
                 (clrhash (action-id->action-of *frame*))
                 (clrhash (callback-id->callback-of *frame*))
                 (emit-into-html-stream buffer-stream
                   (render (root-component-of *frame*)))))
         (headers (with-output-to-sequence (header-stream :element-type '(unsigned-byte 8)
                                                          :initial-buffer-size 128)
                    (setf (header-value self +header/content-length+) (princ-to-string (length body)))
                    (send-http-headers (headers-of self) (cookies-of self) :stream header-stream))))
    ;; TODO use multiplexing when writing to the network stream, including the headers
    (write-sequence headers (network-stream-of *request*))
    (write-sequence body (network-stream-of *request*)))
  (values))

(def (function e) make-redirect-response-with-frame-id-decorated (&optional (frame *frame*))
  (bind ((uri (clone-uri (uri-of *request*))))
    (assert (and frame (not (null (id-of frame)))))
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri frame)
    (make-redirect-response uri)))

(def (function e) make-redirect-response-for-current-application ()
  (bind ((uri (clone-uri (uri-of *request*))))
    (clear-uri-query-parameters uri)
    (setf (path-of uri) (path-prefix-of *application*))
    (decorate-uri uri *application*)
    (decorate-uri uri *session*)
    (decorate-uri uri *frame*)
    (make-redirect-response uri)))
