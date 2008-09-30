(in-package :wui-test)

(defvar *running-test-servers* (list))

(defun start-test-server (server &key maximum-worker-count)
  (when maximum-worker-count
    (setf (maximum-worker-count-of server) maximum-worker-count))
  (finishes
    (is (null (socket-of server)))
    (write-string (test-server-info-string server) *debug-io*)
    (startup-server server)
    (unless (zerop (maximum-worker-count-of server))
      (is (not (null (socket-of server))))
      (pushnew server *running-test-servers*)
      (setf *test-server* server))))

(defun stop-test-server (&optional (server *test-server*))
  (finishes
    (is (not (null server)))
    (is (not (null (socket-of server))))
    (shutdown-server server)
    (is (null (socket-of server)))
    (setf *running-test-servers* (delete server *running-test-servers*))
    (when (eq server *test-server*)
      (setf *test-server* nil))
    (values)))

(defixture ensure-test-server
  (:setup
   (start-test-server (make-instance 'server :host *test-host* :port *test-port*)))
  (:teardown
   (stop-test-server)))

(defun start-test-server-with-handler (handler &rest args &key (server-type 'server) &allow-other-keys)
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host *test-host* :port *test-port* args)))
    (setf (handler-of server) handler)
    (start-test-server-and-wait server)))

(defun start-test-server-with-brokers (brokers &rest args &key
                                       (server-type 'broker-based-server)
                                       (host *test-host*)
                                       (port *test-port*)
                                       (maximum-worker-count 16) ; lower to 0 to start in the REPL thread
                                       &allow-other-keys)
  (when *test-server*
    (cerror "Start anyway" "*TEST-SERVER* is not NIL which means that there's a test server still running. You can use STOP-TEST-SERVER to shut it down. See also *RUNNING-TEST-SERVERS*."))
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host host :port port :maximum-worker-count maximum-worker-count args)))
    (setf (brokers-of server) (ensure-list brokers))
    (start-test-server server)
    server))

(def function test-server-info-string (server)
  (bind ((host (host-of server))
         (port (port-of server))
         (uri (make-uri :scheme "http" :host host :port port))
         (uri-string (print-uri-to-string uri)))
    (format nil "Server running at ~A. You can use STOP-TEST-SERVER to stop it. See also *TEST-SERVER* and *RUNNING-TEST-SERVERS*.~%~
                 You may stress test it with something like:~%~
                 siege -c100 -t10S ~A (add -b for full throttle benchmarking)~%~
                 httperf --rate 1000 --num-conn 10000 --port ~A --server ~A --uri /foo/bar/~%~
                 "
            uri-string
            uri-string
            port
            host)))

(defun start-test-server-and-wait (server)
  (unwind-protect
       (progn
         (start-test-server server)
         (break (test-server-info-string server)))
    (stop-test-server server)))

(defun start-request-echo-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-test-server-with-handler (lambda ()
                                      (send-response (make-request-echo-response)))
                                    :maximum-worker-count maximum-worker-count)))

(defun start-project-file-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-test-server-with-brokers (make-file-serving-broker "/wui/" (project-relative-pathname ""))
                                    :maximum-worker-count maximum-worker-count)))

(def (function o) functional-response-broker (request)
  (make-functional-response ()
    (emit-simple-html-document-response (:title "foo")
      <p "bar">)))

(defun start-functional-response-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-test-server-with-brokers 'functional-response-broker
                                    :maximum-worker-count maximum-worker-count)))
