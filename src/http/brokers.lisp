;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (constant e :test (constantly t)) +request-echo-response+ (make-instance 'request-echo-response))
(def (constant e :test (constantly t)) +no-handler-response+   (make-instance 'no-handler-response))

(defgeneric handle-request (thing request))

(def (class* e) broker-based-server (server)
  ((brokers))
  (:default-initargs :handler 'broker-based-server-handler))

(def function broker-based-server-handler ()
  (handle-request *server* *request*))

(def method handle-request :around ((server server) (request request))
  (bind ((start-time (get-internal-real-time))
         (remote-address (remote-address-of request))
         (raw-uri (raw-uri-of request)))
    (http.info "Handling request from ~S for ~S" remote-address raw-uri)
    (call-next-method)
    (bind ((seconds (/ (- (get-internal-real-time) start-time)
                       internal-time-units-per-second)))
      (when (> seconds 0.05)
        (http.info "Handled request in ~,3f secs (request came from ~S for ~S)" seconds remote-address raw-uri)))))

(def method handle-request ((server broker-based-server) (request request))
  (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) server))))
  (bind ((response (or (query-brokers-for-response request (brokers-of server))
                       +no-handler-response+)))
    (assert (typep response 'response))
    (send-response response)))

(def (function o) query-brokers-for-response (initial-request initial-brokers)
  (or (iterate-brokers-for-response (lambda (broker request)
                                      (funcall broker request))
                                    initial-request
                                    initial-brokers
                                    initial-brokers
                                    0)
      +no-handler-response+))

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
            (result (funcall visitor broker request)))
       (typecase result
         (response
          ;; yay! return with the response...
          result)
         (cons
          ;; recurse with a new set of brokers provided by the previous broker.
          ;; also record the broker who provided the last set of brokers.
          (bind ((*brokers* (list* broker *brokers*)))
            (iterate-brokers-for-response visitor request initial-brokers result (1+ recursion-depth))))
         (request
          ;; we've got a new request, start over using the new set of brokers
          (iterate-brokers-for-response visitor result initial-brokers initial-brokers (1+ recursion-depth)))
         (t
          (iterate-brokers-for-response visitor request initial-brokers (rest brokers) recursion-depth)))))))


;;;;;;;;;;;
;;; brokers

(defgeneric matches-request? (broker request)
  (:method (broker request)
    #f))

(defgeneric produce-response (broker request))

(def class* broker ()
  ()
  (:metaclass funcallable-standard-class))

(def constructor broker
  (set-funcallable-instance-function
    self (lambda (request)
           (default-broker-handler self request))))

(def function default-broker-handler (broker request)
  (when (matches-request? broker request)
    (produce-response broker request)))

(def class* broker-with-path (broker)
  ((path))
  (:metaclass funcallable-standard-class))

(defmethod matches-request? ((broker broker-with-path) request)
  (matches-request-uri-path? (path-of broker) request))

(def (function o) matches-request-uri-path? (path request)
  (bind ((path (etypecase path
                 (string path)
                 (broker-with-path (path-of path))))
         (query-path (path-of (uri-of request))))
    (string= path query-path)))

(def class* broker-with-path-prefix (broker)
  ((path-prefix))
  (:metaclass funcallable-standard-class))

(defmethod matches-request? ((broker broker-with-path-prefix) request)
  (matches-request-uri-path-prefix? (path-of broker) request))

(def (function o) matches-request-uri-path-prefix? (path-prefix request)
  (bind ((path-prefix (etypecase path-prefix
                        (string path-prefix)
                        (broker-with-path-prefix (path-prefix-of path-prefix))))
         (query-path (path-of (uri-of request)))
         ((:values matches? relative-path) (starts-with-subseq path-prefix query-path :return-suffix t)))
    (values matches? relative-path)))

(def class* constant-response-broker (broker)
  ((response))
  (:metaclass funcallable-standard-class))

(defmethod produce-response ((broker constant-response-broker) request)
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

