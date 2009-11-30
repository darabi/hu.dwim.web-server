;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (class* e) broker-based-server (server)
  ((brokers :export :accessor :type sequence)))

(def print-object broker-based-server
  (print-object/server -self-)
  (write-string "; brokers: ")
  (princ (length (brokers-of -self-))))

(def method handle-request ((server broker-based-server) (request request))
  (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) server))))
  (bind ((result (multiple-value-list (query-brokers-for-response request (brokers-of server))))
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
                                                   (funcall (etypecase broker
                                                              (broker (handler-of broker))
                                                              (function-designator broker))
                                                            :broker broker
                                                            :request request))
                                                 initial-request
                                                 initial-brokers
                                                 initial-brokers
                                                 0))))
    (if (first results)
        (progn
          (processed-request-counter/increment answering-broker)
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
          (bind ((*brokers* (cons broker *brokers*)))
            (iterate-brokers-for-response visitor request initial-brokers result (1+ recursion-depth))))
         (request
          ;; we've got a new request, start over using the original set of brokers
          (iterate-brokers-for-response visitor result initial-brokers initial-brokers (1+ recursion-depth)))
         (t
          (iterate-brokers-for-response visitor request initial-brokers (rest brokers) recursion-depth)))))))


;;;;;;
;;; broker

(def definer broker (name supers slots &rest options)
  (bind ()
    ;; kept commented out to have something to copy/paste when an extra option is introduced...
    ;; (handler (second (assoc :handler options)))
    ;; (setf options (remove-if [member !1 '(:handler)] options :key 'first))
    (with-standard-definer-options name
      `(def class* ,name ,(or supers '(broker))
         ,slots
         ,@options))))

(def class* broker (request-counter-mixin)
  ((priority 0 :type number)
   (handler 'broker/default-handler :type function-designator)))

(def constructor broker
  (unless (priority-of -self-)
    (setf (priority-of -self-) 0)))

;; the default handler of brokers start a new generic protocol to introduce a customizable point of filtering
(def function broker/default-handler (&key broker request &allow-other-keys)
  (call-if-matches-request broker request
                           (lambda ()
                             (bind ((*brokers* (cons broker *brokers*)))
                               (produce-response broker request)))))

;;;;;;
;;; broker-with-path

(def class* broker-with-path (broker)
  ((path)))

(def print-object broker-with-path
  (format t "~S ~S" (path-of -self-) (priority-of -self-)))

(def method call-if-matches-request ((broker broker-with-path) request thunk)
  (bind ((path (path-of broker))
         (remaining-query-path (remaining-path-of-request-uri request)))
    (server.debug "Trying to match ~A; path is ~S, remaining-query-path is ~S" broker path remaining-query-path)
    (when (string= path remaining-query-path)
      (with-new-matching-uri-path-element path
        (funcall thunk)))))

;;;;;;
;;; broker-with-path-prefix

(def class* broker-with-path-prefix (broker)
  ((path-prefix :type string)))

(def print-object broker-with-path-prefix
  (format t "~S ~S" (path-prefix-of -self-) (priority-of -self-)))

(def method call-if-matches-request ((broker broker-with-path-prefix) request thunk)
  (bind ((path-prefix (path-prefix-of broker))
         (remaining-query-path (remaining-path-of-request-uri request)))
    (server.debug "Trying to match ~A; path-prefix is ~S, remaining-query-path is ~S" broker path-prefix remaining-query-path)
    (when (starts-with-subseq path-prefix remaining-query-path)
      (with-new-matching-uri-path-element path-prefix
        (funcall thunk)))))

;;;;;;
;;; constant-response-broker

(def class* constant-response-broker (broker)
  ((response)))

(def method handle-request ((broker constant-response-broker) request)
  (response-of broker))

(def class* constant-response-broker-at-path (constant-response-broker broker-with-path)
  ())

(def class* constant-response-broker-at-path-prefix (constant-response-broker broker-with-path-prefix)
  ())

(def (function e) make-redirect-broker (path target-uri)
  (make-instance 'constant-response-broker-at-path
                 :path path
                 :response (make-redirect-response target-uri)))

(def (function e) make-path-prefix-redirect-broker (path-prefix target-uri)
  (make-instance 'constant-response-broker-at-path-prefix
                 :path-prefix path-prefix
                 :response (make-redirect-response target-uri)))

;;;;;;
;;; functional-broker

(def class* functional-broker (broker)
  ())

(def (macro e) make-functional-broker (&body body)
  (with-unique-names (broker)
    `(bind ((,broker (make-instance 'functional-broker)))
       (setf (handler-of ,broker) (named-lambda functional-broker/body (&key ((:request -request-)) &allow-other-keys)
                                    (declare (ignorable -request-))
                                    (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) ,broker))))
                                    ,@body))
       ,broker)))

;;;;;;
;;; delegate-broker

(def class* delegate-broker (broker)
  ((children nil))
  (:documentation "A simple broker that delegates the handling of a request to one of its children. Base class for things like a virtual host dispatcher broker."))

(def method call-if-matches-request ((broker delegate-broker) request thunk)
  (error "CALL-IF-MATCHES-REQUEST reached for delegate-broker ~A" broker))

(def method handle-request ((broker delegate-broker) request)
  (debug-only (assert (and (boundp '*brokers*) (eq (first *brokers*) broker))))
  ;; tell ITERATE-BROKERS-FOR-RESPONSE to go on with a new set of brokers
  (children-of broker))

(def class* filtered-delegate-broker (delegate-broker)
  ((filter :type function-designator :documentation "A function that is (funcall filter request)'d, and it should return T for requests that are supposed to be further processed by the child brokers of this FILTERED-DELEGATE-BROKER.")))

(def method call-if-matches-request ((broker filtered-delegate-broker) request thunk)
  (if (funcall (filter-of broker) request)
      (funcall thunk)
      (values)))

(def (function e) make-virtual-host-delegate-broker (host-name &rest child-brokers)
  "(setf (brokers-of *my-server*) (list (make-virtual-host-delegate-broker \"localhost.localdomain\"
                                                                           (make-functional-broker (make-request-echo-response)))
                                        *my-application*))
will echo the request when the host of the http request url ends with \"localhost.localdomain\", otherwise it goes on with *my-application*."
  (make-instance 'filtered-delegate-broker
                 :filter (named-lambda virtual-host-filter (request)
                           (ends-with-subseq host-name (host-of (uri-of request))))
                 :children child-brokers))