;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def condition* broker-recursion-limit-reached (request-processing-error)
  ((brokers :documentation "The stack of brokers we visited while reaching the limit"))
  (:report
   (lambda (error stream)
     (format stream "Broker recursion limit reached while calling brokers. Broker path: ~A~%" (brokers-of error)))))

(def function broker-recursion-limit-reached (&optional (brokers *brokers*))
  (error 'broker-recursion-limit-reached :request *request* :brokers brokers))

(def condition* too-many-sessions (request-processing-error)
  ((application nil))
  (:report
   (lambda (error stream)
     (format stream "Too many active session for application ~S, the server seems to be overloaded."
             (application-of error)))))

(def function too-many-sessions (application)
  (error 'too-many-sessions :request *request* :application application))
