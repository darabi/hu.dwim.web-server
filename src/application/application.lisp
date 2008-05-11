;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) make-new-session (application))

(def (special-variable e) *maximum-number-of-sessions-per-application* most-positive-fixnum
  "The default for the same slot in applications.")

(def (special-variable e) *application*)

(def (function e) make-application (&key (path-prefix ""))
  (make-instance 'application :path-prefix path-prefix))

(def class* application (broker-with-path-prefix)
  ((entry-points nil)
   (session nil)
   (session-class)
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
          -body-))
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

(def (function o) application-handler (application request)
  (bind ((*application* application)
         (session nil))
    (with-lock-held-on-application application
      (setf session (find-application-session-from-request application)))
    ;; TODO frame
    (bind ((*session* session))
      (flet ((body ()
               (bind (((:values response post-session-callbacks)
                       (handle-request application request)))
                 (values response post-session-callbacks))))
        (bind ((response (if session
                             (bind ((response nil)
                                    (post-session-callback nil))
                               (with-lock-held-on-session session
                                 (setf (values response post-session-callback) (body)))
                               (awhen post-session-callback
                                 (funcall it))
                               response)
                             (bind (((:values response post-session-callback) (body)))
                               (awhen post-session-callback
                                 (funcall it))
                               response))))
          (when response
            (bind ((request-uri (uri-of request)))
              (app.debug "Decorating response with the session cookie for session ~S" (session-of application))
              (add-cookie (make-cookie
                           +session-id-parameter-name+
                           (aif (session-of application)
                                (id-of it)
                                "")
                           :comment "WUI session id"
                           :domain (concatenate-string "." (host-of request-uri))
                           :path (path-prefix-of application))
                          response)))
          response)))))

(defmethod handle-request ((application application) request)
  (bind (((:values matches? relative-path) (matches-request-uri-path-prefix? application request)))
    (when matches?
      (app.debug "~A matched with relative-path ~S, querying entry-points for response" application relative-path)
      (query-entry-points-for-response application request relative-path))))

(def (function o) query-entry-points-for-response (application initial-request relative-path)
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
        +no-handler-response+)))

(def (generic e) session-class (application)
  (:documentation "Returns a list of the session mixin classes.

Custom implementations should look something like this:
\(defmethod session-class list \(\(app your-application))
  'your-session-mixin)")
  (:method-combination list))

(defmethod session-class list ((application application))
  'session)

(defmethod make-new-session :around (application)
  (with-thread-name " / MAKE-NEW-SESSION"
    (call-next-method)))

(defmethod make-new-session ((application application))
  (assert-application-lock-held application)
  (bind ((session-id->session (session-id->session-of application)))
    (when (> (hash-table-count session-id->session)
             (maximum-number-of-sessions-of application))
      (too-many-sessions application))
    (bind (((:values session-id session)
            (insert-with-new-random-hash-table-key session-id->session +session-id-length+
                                                   (make-instance (session-class-of application)))))
      (setf (id-of session) session-id)
      (awhen (session-of application)
        (notify-session-expiration application it)
        (remhash (id-of it) session-id->session))
      (setf (session-of application) session)
      (app.dribble "New session with id ~S" (id-of session))
      session)))
