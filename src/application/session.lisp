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

(def class* session ()
  ((id nil :type string)
   (last-activity-at (get-internal-real-time))
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

(def class* frame ()
  ((last-activity-at (get-internal-real-time))
   (frame-index 0)
   (action-id->action (make-hash-table :test 'equal))))

(def (generic e) notify-session-expiration (application session)
  (:method (application (session session))
    ;; nop
    ))

(def function notify-session-activity (session)
  (setf (last-activity-at-of session) (get-internal-real-time)))

(def function notify-frame-activity (frame)
  (setf (last-activity-at-of frame) (get-internal-real-time)))

(def (function o) find-application-session-from-request (application)
  (bind (;;(request-uri (uri-of *request*))
         (session-id (cookie-value +session-id-parameter-name+
                                   ;; TODO how does it work? domain is not sent with the requests
                                   ;;:domain (host-of request-uri)
                                   )))
    (when session-id
      (bind ((session (gethash session-id (session-id->session-of application))))
        ;; TODO expiration
        session))))
