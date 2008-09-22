(in-package :wui-test)

(defvar *running-test-servers* (list))

(defun startup-test-server (&optional (server *test-server*))
  (finishes
    (is (null (socket-of server)))
    (startup-server server)
    (is (not (null (socket-of server))))
    (pushnew server *running-test-servers*)
    (setf *test-server* server)))

(defun shutdown-test-server (&optional (server *test-server*))
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
   (startup-test-server (make-instance 'server :host *test-host* :port *test-port*)))
  (:teardown
   (shutdown-test-server)))

(defun start-server-with-handler (handler &rest args &key (server-type 'server) &allow-other-keys)
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host *test-host* :port *test-port* args)))
    (setf (handler-of server) handler)
    (start-server-and-wait server)))

(defun start-server-with-brokers (brokers &rest args &key
                                  (server-type 'broker-based-server)
                                  (host *test-host*)
                                  (port *test-port*)
                                  &allow-other-keys)
  (remove-from-plistf args :server-type)
  (bind ((server (apply #'make-instance server-type :host host :port port args)))
    (setf (brokers-of server) (ensure-list brokers))
    (start-server-and-wait server)))

(defun start-server-and-wait (server)
  (bind ((host (host-of server))
         (port (port-of server))
         (uri (make-uri :scheme "http" :host host :port port))
         (uri-string (print-uri-to-string uri)))
    (unwind-protect
         (progn
           (startup-test-server server)
           (break "Server running at ~A. Continue the debugger to stop it.~%~
                   You may stress test it with something like:~%~
                   siege -c100 -t10S ~A (add -b for full throttle benchmarking)~%~
                   httperf --rate 1000 --num-conn 10000 --port ~A --server ~A --uri /foo/bar/~%~
                   "
                  uri-string
                  uri-string
                  port
                  host))
      (shutdown-test-server server))))

(defun start-request-echo-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-handler (lambda ()
                                 (send-response (make-request-echo-response)))
                               :maximum-worker-count maximum-worker-count)))

(defun start-project-file-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-brokers (make-file-serving-broker "/wui/" (project-relative-pathname ""))
                               :maximum-worker-count maximum-worker-count)))

(def (function o) functional-response-broker (request)
  (make-functional-response ()
    (emit-simple-html-document-response (:title "foo")
      <p "bar">)))

(defun start-functional-response-server (&key (maximum-worker-count 16) (log-level +dribble+))
  (with-logger-level wui log-level
    (start-server-with-brokers 'functional-response-broker
                               :maximum-worker-count maximum-worker-count)))
