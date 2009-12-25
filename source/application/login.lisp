;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; login-data

(def (class* ea) login-data ()
  ((extra-arguments '()))
  (:documentation "A login-data is an object that encapsulates information that should be used for authentication. It can be used for dispatching in later phases of the authentication. The EXTRA-ARGUMENTS slot can hold some &rest keyword arguments that is useful later on."))

(def (class* ea) identifier-and-password-login-data (login-data)
  ((identifier)
   (password)))

(def print-object identifier-and-password-login-data
  (write-string "identifier: ")
  (write (identifier-of -self-)))

;;;;;;
;;; application-with-login-support

(def (class* e) application-with-login-support (application)
  ())

(def (class* ea) session-with-login-support (session)
  ((authenticate/return-value nil)))

(def method session-class list ((application application-with-login-support))
  'session-with-login-support)

(def method is-logged-in? ((session session-with-login-support))
  (not (null (authenticate/return-value-of session))))

(def method login ((application application-with-login-support) session login-data)
  (bind ((new-session? #f))
    (app.debug "Login entry point called with login-data ~A" login-data)
    (if session
        (when (is-logged-in? session)
          (error "~S was called while the web session ~A is already authenticated to ~A" 'login session (authenticate/return-value-of session)))
        (progn
          (app.debug "~S is making a new web session" 'login)
          (setf session (make-new-session application))
          (setf new-session? #t)))
    (bind ((authenticate/return-value (authenticate application session login-data)))
      (app.debug "~S returned ~S" 'authenticate authenticate/return-value)
      (setf (authenticate/return-value-of session) authenticate/return-value)
      (when authenticate/return-value
        (setf *session* session)
        (when new-session?
          (app.debug "Registering new web session ~A" session)
          (with-lock-held-on-application (application)
            (register-session application session)))))
    (is-logged-in? session)))

(def method logout :after ((application application) (session session))
  (assert (eq *session* session))
  (mark-session-invalid session)
  ;; set *session* to nil so that the session cookie removal is decorated on the response. otherwise the next request to an entry point
  ;; would send up a session id to an invalid session and trigger HANDLE-REQUEST-TO-INVALID-SESSION.
  (setf *session* nil))

