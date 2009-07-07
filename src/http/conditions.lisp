;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

#|

TODO delme?

(def (condition* e) no-handler-for-request (error)
  ((raw-uri nil)
   (request (when (boundp '*request*)
              *request*)))
  (:report (lambda (error stream)
             (format stream "No handler for query: ~S~%" (or (raw-uri-of error)
                                                             (awhen (request-of error)
                                                               (raw-uri-of it))
                                                             "#<unknown>")))))

(def function no-handler-for-request (&optional (request *request*))
  (error 'no-handler-for-request
         :request request))
|#

(def condition* request-processing-error (error)
  ((request (when (boundp '*request*)
              *request*))))

(def (condition* e) request-content-length-limit-reached (request-processing-error)
  ((content-length nil)
   (content-length-limit *request-content-length-limit*))
  (:report (lambda (error stream)
             (format stream "The content-length of the request is larger than allowed by the server policy (~A > ~A), see *REQUEST-CONTENT-LENGTH-LIMIT*"
                     (content-length-of error)
                     (content-length-limit-of error)))))

(def function request-content-length-limit-reached (content-length &optional (content-length-limit *request-content-length-limit*))
  (error 'request-content-length-limit-reached
         :content-length content-length
         :content-length-limit content-length-limit))

(def condition* broker-recursion-limit-reached (request-processing-error)
  ((brokers :documentation "The stack of brokers we visited while reaching the limit"))
  (:report
   (lambda (error stream)
     (format stream "Broker recursion limit reached while calling brokers. Broker path: ~A~%" (brokers-of error)))))

(def function broker-recursion-limit-reached (&optional (brokers *brokers*))
  (error 'broker-recursion-limit-reached :request *request* :brokers brokers))


(def (condition e) access-denied-error (request-processing-error)
  ())

(def (function e) access-denied-error ()
  (error 'access-denied-error))

(def condition dos-attack-detected (simple-error request-processing-error)
  ())

(def function dos-attack-detected (&optional (format-control "Denial of Service attack?") &rest format-arguments)
  (error 'dos-attack-detected :format-control format-control :format-arguments format-arguments))


(def condition* too-many-sessions (request-processing-error)
  ((application nil))
  (:report
   (lambda (error stream)
     (format stream "Too many active session for application ~S, the server seems to be overloaded."
             (application-of error)))))

(def function too-many-sessions (application)
  (error 'too-many-sessions :request *request* :application application))

