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
  (debug-only (assert (and (boundp '*broker-stack*) (eq (first *broker-stack*) server))))
  (bind ((result (multiple-value-list (query-brokers-for-response request (brokers-of server))))
         (response (first result)))
    (assert (typep response 'response))
    (unwind-protect
         (send-response response)
      (close-response response))
    (values-list result)))

(def method startup-server/with-lock-held ((server broker-based-server) &key &allow-other-keys)
  (setf (brokers-of server) (stable-sort (brokers-of server) 'compare-brokers-for-sorting))
  ;; notify the brokers about the startup
  (dolist (broker (brokers-of server))
    (startup-broker broker))
  (call-next-method))

(def method shutdown-server ((server broker-based-server) &key &allow-other-keys)
  (dolist (broker (brokers-of server))
    (shutdown-broker broker))
  (call-next-method))

(def (function o) query-brokers-for-response (initial-request initial-brokers &key (otherwise [make-not-found-response]))
  (bind ((answering-broker nil)
         (results (multiple-value-list
                   (iterate-brokers-for-response (lambda (broker request)
                                                   (setf answering-broker broker)
                                                   (etypecase broker
                                                     (broker (bind ((*broker-stack* (cons broker *broker-stack*)))
                                                               (funcall (handler-of broker)
                                                                        :broker broker
                                                                        :request request)))
                                                     (function-designator (funcall broker
                                                                                   :broker broker
                                                                                   :request request))))
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
     (broker-recursion-limit-reached request brokers)
     nil)
    (t
     (bind ((broker (first brokers))
            (result-values (multiple-value-list (funcall visitor broker request)))
            (result (first result-values)))
       (typecase result
         (response
          (server.debug "Broker ~A returned response ~A, returning from ITERATE-BROKERS-FOR-RESPONSE" broker result)
          (values-list result-values))
         (cons
          ;; record the broker who provided the new set of brokers on the *broker-stack*
          (bind ((*broker-stack* (cons broker *broker-stack*)))
            (server.debug "Broker ~A returned the new rules ~S, calling ITERATE-BROKERS-FOR-RESPONSE recursively" broker result)
            (iterate-brokers-for-response visitor request initial-brokers result (1+ recursion-depth))))
         (request
          (server.debug "Broker ~A returned the new request ~S, calling ITERATE-BROKERS-FOR-RESPONSE recursively" broker result)
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

(def method handle-request ((broker broker) (request request))
  (call-if-matches-request broker request
                           (lambda ()
                             (produce-response broker request))))

;; the default handler of brokers start a new generic protocol to introduce a customizable point of filtering
(def function broker/default-handler (&key broker request &allow-other-keys)
  (handle-request broker request))

;;;;;;
;;; broker-at-path

(def function broker-path-or-path-prefix (broker &key (otherwise :error))
  (typecase broker
    (broker-at-path (path-of broker))
    (broker-at-path-prefix (path-prefix-of broker))
    (t (handle-otherwise otherwise))))

(def class* broker-at-path (broker)
  ((path)))

(def print-object broker-at-path
  (format t "~S ~S" (path-of -self-) (priority-of -self-)))

(def method call-if-matches-request ((broker broker-at-path) request thunk)
  (bind ((path (path-of broker))
         (remaining-query-path (remaining-path-of-request-uri request)))
    (server.debug "Trying to match ~A; path is ~S, remaining-query-path is ~S" broker path remaining-query-path)
    (when (string= path remaining-query-path)
      (with-new-matching-uri-path-element path
        (funcall thunk)))))

;;;;;;
;;; broker-at-path-prefix

(def class* broker-at-path-prefix (broker)
  ((path-prefix :type string)))

(def print-object broker-at-path-prefix
  (format t "~S ~S" (path-prefix-of -self-) (priority-of -self-)))

(def method call-if-matches-request ((broker broker-at-path-prefix) request thunk)
  (bind ((path-prefix (path-prefix-of broker))
         (remaining-query-path (remaining-path-of-request-uri request)))
    (server.debug "Trying to match ~A; path-prefix is ~S, remaining-query-path is ~S" broker path-prefix remaining-query-path)
    (when (starts-with-subseq path-prefix remaining-query-path)
      (with-new-matching-uri-path-element path-prefix
        (funcall thunk)))))

;;;;;;
;;; compare-brokers-for-sorting

(def (generic e) compare-brokers-for-sorting (a b)
  (:documentation "Used as sorting predicate to sort brokers of a broker-based-server.")
  (:method ((a broker) (b broker))
    (bind ((a/priority (priority-of a))
           (b/priority (priority-of b))
           (a/path (broker-path-or-path-prefix a :otherwise nil))
           (b/path (broker-path-or-path-prefix b :otherwise nil)))
      (cond
        ((and (= a/priority b/priority)
              a/path
              b/path)
         (if (string= a/path b/path)
             (and (not (typep a 'broker-at-path-prefix))
                  (typep b 'broker-at-path-prefix))
             (string> a/path b/path)))
      (t
       (> a/priority b/priority))))))

;;;;;;
;;; constant-response-broker

(def class* constant-response-broker (broker)
  ((response)))

(def method produce-response ((broker constant-response-broker) request)
  (response-of broker))

(def class* constant-response-broker-at-path (constant-response-broker broker-at-path)
  ())

(def class* constant-response-broker-at-path-prefix (constant-response-broker broker-at-path-prefix)
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
                                    (debug-only (assert (and (boundp '*broker-stack*) (eq (first *broker-stack*) ,broker))))
                                    ,@body))
       ,broker)))

;;;;;;
;;; delegate-broker

(def class* delegate-broker (broker)
  ((children nil))
  (:documentation "A simple broker that delegates the handling of a request to one of its children. Base class for things like a virtual host dispatcher broker."))

(def method call-if-matches-request ((broker delegate-broker) request thunk)
  (error "CALL-IF-MATCHES-REQUEST of delegate-broker ~A has been reached" broker))

(def method handle-request ((broker delegate-broker) request)
  (debug-only (assert (and (boundp '*broker-stack*) (eq (first *broker-stack*) broker))))
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
