;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def class* application (broker-with-path-prefix)
  ((entry-points nil))
  (:metaclass funcallable-standard-class))

(def constructor application
  (set-funcallable-instance-function
    self (lambda (request)
           (handle-request self request))))

(def (function e) make-application (&key (path-prefix ""))
  (make-instance 'application :path-prefix path-prefix))

(defmethod handle-request ((application application) request)
  (bind (((:values matches? relative-path) (matches-request-uri-path-prefix? application request)))
    (when matches?
      (app.debug "~A matched with relative-path ~S, querying entry-points for response" application relative-path)
      (query-entry-points-for-response application request relative-path))))

(def (function o) query-entry-points-for-response (application initial-request relative-path)
  (or (iterate-brokers-for-response (lambda (broker request)
                                      (funcall broker request application relative-path))
                                    initial-request
                                    (entry-points-of application)
                                    (entry-points-of application)
                                    0)
      +no-handler-response+))
