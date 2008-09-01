;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +session-cookie-name+ "sid")

(def constant +session-id-length+ 40)

(def (special-variable e) *session*)

(def class* activity-monitor-mixin ()
  ((last-activity-at (get-monotonic-time))
   (time-to-live)))

(def (generic e) notify-activity (thing)
  (:method ((self activity-monitor-mixin))
    (setf (last-activity-at-of self) (get-monotonic-time))))

(def (function e) mark-expired (thing)
  (setf (last-activity-at-of thing) most-negative-fixnum))

(def (generic e) time-since-last-activity (thing)
  (:method ((self activity-monitor-mixin))
    (- (get-monotonic-time) (last-activity-at-of self))))

(def generic is-timed-out? (thing)
  (:method ((self activity-monitor-mixin))
    (> (time-since-last-activity self)
       (time-to-live-of self))))

(def class* string-id-mixin ()
  ((id nil :type string)))

(def class* string-id-for-funcallable-mixin ()
  ((id nil :type string))
  (:metaclass funcallable-standard-class))

(def print-object (string-id-mixin :identity #t :type #f)
  (print-object-for-string-id-mixin -self-))

(def print-object (string-id-for-funcallable-mixin :identity #t :type #f)
  (print-object-for-string-id-mixin -self-))

(def function print-object-for-string-id-mixin (self)
  (write-string (string (class-name (class-of self))))
  (write-string " ")
  (aif (id-of self)
       (write-string it)
       "<no id yet>"))


;;;;;;;;;;;
;;; session

(def class* session (string-id-mixin activity-monitor-mixin)
  ((application nil)
   (client-timezone (default-timezone-of *application*))
   (current-frame-index 0)
   (unique-dom-id-counter 0)
   (frame-id->frame (make-hash-table :test 'equal))
   (lock nil)))

(def with-macro* with-lock-held-on-session (session)
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-session for ~S in thread ~S" session (current-thread))
        (with-recursive-lock-held ((lock-of session))
          (-body-)))
    (threads.dribble "Leaving with-lock-held-on-session for ~S in thread ~S" session (current-thread))))

(defmethod (setf id-of) :before (id (session session))
  (awhen (id-of session)
    (error "The session ~S already has an id: ~A." session it))
  (assert (null (lock-of session)))
  (setf (lock-of session) (make-recursive-lock (format nil "Session lock for session ~A" id))))

(def (function i) assert-session-lock-held (session)
  (assert (is-lock-held? (lock-of session)) () "You must have a lock on the session here"))

(def (generic e) notify-session-expiration (application session)
  (:method (application (session session))
    ;; nop
    ))

(def (function o) find-session-from-request (application)
  (bind ((session-id (cookie-value +session-cookie-name+)))
    (when session-id
      (app.debug "Found session-id parameter ~S" session-id)
      (bind ((session (gethash session-id (session-id->session-of application))))
        (if (and session
                 (not (is-timed-out? session)))
            (progn
              (app.debug "Looked up as valid session ~A" session)
              session)
            (values))))))

