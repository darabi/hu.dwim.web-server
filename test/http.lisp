(in-package :hu.dwim.wui.test)

(def special-variable *running-test-servers* (list))

(def function start-test-server (server &key maximum-worker-count)
  (when maximum-worker-count
    (setf (maximum-worker-count-of server) maximum-worker-count))
  (finishes
    (is (not (is-server-running? server)))
    (write-string (test-server-info-string server) *debug-io*)
    (startup-server server)
    (unless (zerop (maximum-worker-count-of server))
      (is (is-server-running? server))
      (pushnew server *running-test-servers*)
      (setf *test-server* server))))

(def function stop-test-server (&optional (server *test-server*))
  (finishes
    (is (not (null server)))
    (dolist (listen-entry (listen-entries-of server))
      (is (not (null (socket-of listen-entry)))))
    (shutdown-server server)
    (dolist (listen-entry (listen-entries-of server))
      (is (null (socket-of listen-entry))))
    (setf *running-test-servers* (delete server *running-test-servers*))
    (when (eq server *test-server*)
      (setf *test-server* nil))
    (values)))

(def fixture ensure-test-server
  (:setup
   (start-test-server (make-instance 'server :host *test-host* :port *test-port*)))
  (:teardown
   (stop-test-server)))

(def function start-test-server-with-handler (handler &rest args &key (server-type 'server) &allow-other-keys)
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host *test-host* :port *test-port* args)))
    (setf (handler-of server) handler)
    (start-test-server-and-wait server)))

(def function start-test-server-with-brokers (brokers &rest args &key
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
  (bind ((listen-entry (first (listen-entries-of server)))
         (host (address-to-string (host-of listen-entry)))
         (port (port-of listen-entry))
         (uri (make-uri :scheme "http" :host host :port port))
         (uri-string (print-uri-to-string uri)))
    (format nil "Server running at ~A. You can use STOP-TEST-SERVER to stop it. See also *TEST-SERVER* and *RUNNING-TEST-SERVERS*.~%~
                 You may stress test it with something like:~%~
                 siege -c100 -t10S ~A (add -b for full throttle benchmarking)~%~
                 httperf --rate 1000 --num-conn 10000 --port ~A --server ~A --uri /foo/bar/~%~
                 "
            server
            uri-string
            port
            host)))

(def function start-test-server-and-wait (server)
  (unwind-protect
       (progn
         (start-test-server server)
         (break (test-server-info-string server)))
    (stop-test-server server)))

(def function start-request-echo-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-test-server-with-handler (lambda ()
                                      (send-response (make-request-echo-response)))
                                    :maximum-worker-count maximum-worker-count)))

(def function start-project-file-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-test-server-with-brokers (make-directory-serving-broker "/wui/" (system-relative-pathname :hu.dwim.wui.test ""))
                                    :maximum-worker-count maximum-worker-count)))

(def function start-functional-response-server (&key (maximum-worker-count 4) (log-level +warn+))
  (with-logger-level wui log-level
    (start-test-server-with-brokers (make-functional-broker
                                      (with-request-params (name)
                                        (make-functional-html-response ()
                                          (with-html-document (:title "foo")
                                            <h3 ,(or name "The name query parameter is not specified!")>))))
                                    :maximum-worker-count maximum-worker-count)))
