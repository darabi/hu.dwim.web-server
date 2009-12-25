;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def condition* too-many-sessions (request-processing-error)
  ((application nil))
  (:report
   (lambda (error stream)
     (format stream "Too many active session for application ~S, the server seems to be overloaded."
             (application-of error)))))

(def function too-many-sessions (application)
  (error 'too-many-sessions :request *request* :application application))

(def condition* login-failed (simple-error)
  ((login-data)))

(def function login-failed (login-data format-control &rest format-arguments)
  (error 'login-failed :format-control format-control :format-arguments format-arguments :login-data login-data))
