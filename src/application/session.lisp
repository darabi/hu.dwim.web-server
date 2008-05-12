;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant :test 'string=) +session-id-parameter-name+  "_sid")
(def (constant :test 'string=) +frame-id-parameter-name+    "_fid")
(def (constant :test 'string=) +frame-index-parameter-name+ "_fix")

(def constant +session-id-length+  32)
(def constant +frame-id-length+    8)

(def (special-variable e) *session*)
(def (special-variable e) *frame*)

(def class* activity-monitor-mixin ()
  ((last-activity-at (get-monotonic-time))
   (time-to-live)))

(def (generic e) notify-activity (thing)
  (:method ((self activity-monitor-mixin))
    (setf (last-activity-at-of self) (get-monotonic-time))))

(def (generic e) time-since-last-activity (thing)
  (:method ((self activity-monitor-mixin))
    (- (get-monotonic-time) (last-activity-at-of self))))

(def generic is-timed-out? (thing)
  (:method ((self activity-monitor-mixin))
    (> (time-since-last-activity self)
       (time-to-live-of self))))


;;;;;;;;;;;
;;; session

(def class* session (activity-monitor-mixin)
  ((application nil)
   (id nil :type string)
   (frame-id->frame (make-hash-table :test 'equal))
   (lock nil)))

(def print-object (session :identity #t :type #f)
  (write-string (string (class-name (class-of self))))
  (write-string " ")
  (aif (id-of self)
       (write-string it)
       "<no id yet>"))

(def with-macro with-lock-held-on-session (session)
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-session for ~S in thread ~S" session (current-thread))
        (with-recursive-lock-held ((lock-of session))
          -body-))
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
  (bind ((session-id (cookie-value +session-id-parameter-name+)))
    (when session-id
      (app.debug "Found session-id ~S" session-id)
      (bind ((session (gethash session-id (session-id->session-of application))))
        (if (and session
                 (not (is-timed-out? session)))
            (progn
              (app.debug "Looked up as valid session ~A" session)
              session)
            (values))))))


;;;;;;;;;
;;; frame

(def class* frame (activity-monitor-mixin)
  ((frame-index 0)
   (action-id->action (make-hash-table :test 'equal))))

(def (function o) find-frame-from-request (session)
  (bind ((frame-id (parameter-value +frame-id-parameter-name+)))
    (when frame-id
      (app.debug "Found frame-id ~S" frame-id)
      (bind ((frame (gethash frame-id (frame-id->frame-of session))))
        (if (and frame
                 (not (is-timed-out? frame)))
            (progn
              (app.debug "Looked up as valid frame ~A" frame)
              frame)
            (values))))))
