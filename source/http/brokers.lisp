;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (class* e) broker-based-server (server)
  ((brokers :export :accessor))
  (:default-initargs :handler 'broker-based-server-handler))

(def print-object broker-based-server
  (print-object/server -self-)
  (write-string "; brokers: ")
  (princ (length (brokers-of -self-))))

(def function broker-based-server-handler ()
  (handle-request *server* *request*))

(def method handle-request ((server broker-based-server) (request request))
  (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) server))))
  (bind ((result (multiple-value-list
                  (or (query-brokers-for-response request (brokers-of server))
                      (make-no-handler-response))))
         (response (first result)))
    (assert (typep response 'response))
    (unwind-protect
         (send-response response)
      (close-response response))
    (values-list result)))

(def (function o) query-brokers-for-response (initial-request initial-brokers &key (otherwise [make-no-handler-response]))
  (bind ((answering-broker nil)
         (results (multiple-value-list
                   (iterate-brokers-for-response (lambda (broker request)
                                                   (setf answering-broker broker)
                                                   (funcall broker request))
                                                 initial-request
                                                 initial-brokers
                                                 initial-brokers
                                                 0))))
    (if (first results)
        (progn
          (incf (processed-request-count-of answering-broker))
          (values-list results))
        (handle-otherwise otherwise))))

(def (function o) iterate-brokers-for-response (visitor request initial-brokers brokers recursion-depth)
  (declare (type fixnum recursion-depth))
  (cond
    ((null brokers)
     nil)
    ((> recursion-depth 32)
     (broker-recursion-limit-reached request)
     nil)
    (t
     (bind ((broker (first brokers))
            (result-values (multiple-value-list (funcall visitor broker request)))
            (result (first result-values)))
       (typecase result
         (response
          ;; yay! return with the response...
          (values-list result-values))
         (cons
          ;; recurse with a new set of brokers provided by the previous broker.
          ;; also record the broker who provided the last set of brokers.
          (bind ((*brokers* (list* broker *brokers*)))
            (iterate-brokers-for-response visitor request initial-brokers result (1+ recursion-depth))))
         (request
          ;; we've got a new request, start over using the original set of brokers
          (iterate-brokers-for-response visitor result initial-brokers initial-brokers (1+ recursion-depth)))
         (t
          (iterate-brokers-for-response visitor request initial-brokers (rest brokers) recursion-depth)))))))


;;;;;;
;;; Brokers

(def generic matches-request? (broker request)
  (:method (broker request)
    #f))

(def generic produce-response (broker request))

(def class* broker (closer-mop:funcallable-standard-object request-counter-mixin)
  ((priority 0 :type number))
  (:metaclass funcallable-standard-class))

(def constructor broker
  (set-funcallable-instance-function
    -self- (lambda (request)
             (default-broker-handler -self- request)))
  (unless (priority-of -self-)
    (setf (priority-of -self-) 0)))

(def function default-broker-handler (broker request)
  (when (matches-request? broker request)
    (produce-response broker request)))

(def class* broker-with-path (broker)
  ((path))
  (:metaclass funcallable-standard-class))

(def print-object broker-with-path
  (format *standard-output* "~S ~S" (path-of -self-) (priority-of -self-)))

(def method matches-request? ((broker broker-with-path) request)
  (request-uri-matches-path? (path-of broker) request))

(def (function o) request-uri-matches-path? (path request)
  (bind ((path (etypecase path
                 (string path)
                 (broker-with-path (path-of path))))
         (query-path (path-of (uri-of request))))
    (string= path query-path)))

(def class* broker-with-path-prefix (broker)
  ((path-prefix :type string))
  (:metaclass funcallable-standard-class))

(def print-object broker-with-path-prefix
  (format *standard-output* "~S ~S" (path-prefix-of -self-) (priority-of -self-)))

(def function broker-path-or-path-prefix-or-nil (broker)
  (typecase broker
    (broker-with-path (path-of broker))
    (broker-with-path-prefix (path-prefix-of broker))
    (t nil)))

(def method matches-request? ((broker broker-with-path-prefix) request)
  (request-uri-matches-path-prefix? (path-of broker) request))

(def (function o) request-uri-matches-path-prefix? (path-prefix request)
  (bind ((path-prefix (etypecase path-prefix
                        (string path-prefix)
                        (broker-with-path-prefix (path-prefix-of path-prefix))))
         (query-path (path-of (uri-of request)))
         ((:values matches? relative-path) (starts-with-subseq path-prefix query-path :return-suffix t)))
    (values matches? relative-path)))

(def class* constant-response-broker (broker)
  ((response))
  (:metaclass funcallable-standard-class))

(def method produce-response ((broker constant-response-broker) request)
  (response-of broker))

(def class* constant-response-broker-at-path (constant-response-broker broker-with-path)
  ()
  (:metaclass funcallable-standard-class))

(def class* constant-response-broker-at-path-prefix (constant-response-broker broker-with-path-prefix)
  ()
  (:metaclass funcallable-standard-class))

(def (function e) make-redirect-broker (path target-uri)
  (make-instance 'constant-response-broker-at-path
                 :path path
                 :response (make-redirect-response target-uri)))

(def (function e) make-path-prefix-redirect-broker (path-prefix target-uri)
  (make-instance 'constant-response-broker-at-path-prefix
                 :path-prefix path-prefix
                 :response (make-redirect-response target-uri)))

(def class* functional-broker (broker)
  ((response-factory))
  (:metaclass funcallable-standard-class))

(def constructor functional-broker
  (set-funcallable-instance-function
    -self- (lambda (request)
             (funcall (response-factory-of -self-) request))))

(def (macro e) make-functional-broker (&body body)
  `(make-instance 'functional-broker
                  :response-factory (named-lambda functional-broker-response-factory (-request-)
                                      (declare (ignorable -request-))
                                      ,@body)))

(def class* delegate-broker (broker)
  ((children ()))
  (:metaclass funcallable-standard-class)
  (:documentation "A simple broker that delegates the handling of a request to one of its children. Base class for things like a virtual host dispatcher broker."))

(def constructor delegate-broker
  (set-funcallable-instance-function
    -self- (lambda (request)
             (delegate-broker-handler -self- request))))

(def function delegate-broker-handler (broker request)
  (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) broker))))
  (query-brokers-for-response request (children-of broker) :otherwise nil))

(def class* filtered-delegate-broker (delegate-broker)
  ((filter :type (function (request) *) :documentation "A function that is funcall'd with the request and it should return T for requests that are supposed to be further processed."))
  (:metaclass funcallable-standard-class))

(def constructor filtered-delegate-broker
  (set-funcallable-instance-function
    -self- (lambda (request)
             (filtered-delegate-handler -self- request))))

(def function filtered-delegate-handler (broker request)
  (if (funcall (filter-of broker) request)
      (delegate-broker-handler broker request)
      nil))

(def (function e) make-virtual-host-broker (host-name &rest child-brokers)
  "(setf (brokers-of *my-server*) (list (make-virtual-host-broker \"localhost.localdomain\"
                                                                  (make-functional-broker (make-request-echo-response)))
                                        *my-application*))
will echo the request when the http request url is \"localhost.localdomain\", otherwise go on for *my-application*."
  (make-instance 'filtered-delegate-broker
                 :filter (named-lambda virtual-host-filter (request)
                           (string= host-name (host-of (uri-of request))))
                 :children (copy-list child-brokers)))
