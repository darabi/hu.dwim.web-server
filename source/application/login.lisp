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
  ((authenticate-return-value nil)))

(def method session-class list ((application application-with-login-support))
  'session-with-login-support)

(def method is-logged-in? ((session session-with-login-support))
  (not (null (authenticate-return-value-of session))))

(def function login-current-session (login-data)
  (bind ((session (login *application* *session* login-data)))
    (check-type session session)
    (setf *session* session)))

(def method login :around ((application application-with-login-support) (session null) login-data)
  (app.debug "~S is making a new web session and calling itself with it" 'login)
  (setf session (make-new-session application))
  (login application session login-data)
  ;; login signals if there's any error
  (app.debug "Registering new web session ~A" session)
  (with-lock-held-on-application (application)
    (register-session application session))
  session)

(def method login ((application application-with-login-support) (session session-with-login-support) login-data)
  (app.debug "~S called with login-data ~A" 'login login-data)
  (assert (not (is-logged-in? session)))
  (bind ((authenticate-return-value (authenticate application session login-data)))
    (app.debug "~S returned ~S" 'authenticate authenticate-return-value)
    (unless authenticate-return-value
      (login-failed login-data "~S returned false" 'authenticate))
    (setf (authenticate-return-value-of session) authenticate-return-value))
  session)

(def function logout-current-session ()
  (logout *application* *session*)
  ;; set *session* to nil so that the session cookie removal is decorated on the response. otherwise the next request to an entry point
  ;; would send up a session id to an invalid session and trigger HANDLE-REQUEST-TO-INVALID-SESSION.
  (setf *session* nil))

(def method logout ((application application-with-login-support) (session session-with-login-support))
  (mark-session-invalid session))
