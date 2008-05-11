;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (class* e) server ()
  ((host)
   (port)
   (host-header-fallback :documentation "Used when parsing the request and there's no Host header sent from the client.")
   (socket nil)
   (handler :type (or symbol function))
   (request-content-length-limit *request-content-length-limit*)
   (lock (make-recursive-lock "WUI server lock"))
   (shutdown-initiated #f :type boolean)
   (workers (make-adjustable-vector 16))
   (maximum-worker-count 16)
   (occupied-worker-count 0)
   (started-at)
   (processed-request-count 0)))

(def with-macro with-lock-held-on-server (server)
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-server for server ~S in thread ~S" server (current-thread))
        (with-recursive-lock-held ((lock-of server))
          -body-))
    (threads.dribble "Leaving with-lock-held-on-server for server ~S in thread ~S" server (current-thread))))

(def (generic e) startup-server (server &key &allow-other-keys))
(def (generic e) shutdown-server (server &key &allow-other-keys))

(defmethod startup-server ((server server) &key (initial-worker-count 2) &allow-other-keys)
  (assert (host-of server))
  (assert (port-of server))
  (setf (shutdown-initiated-p server) #f)
  (setf (host-header-fallback-of server)
        (print-uri-to-string (make-uri :scheme "http"
                                       :host (host-of server)
                                       :port (port-of server))))
  (with-lock-held-on-server server
    (loop
       (with-simple-restart (retry "Try opening the socket again on host ~S port ~S" (host-of server) (port-of server))
         (server.debug "Binding socket to host ~A, port ~A" (host-of server) (port-of server))
         (bind ( ;;(net.sockets:*ipv6* nil) ; TODO: temporarily disable ipv6 because it fails
                (socket-is-ok nil)
                (socket (make-socket :connect :passive
                                     :local-host (host-of server)
                                     :local-port (port-of server)
                                     :external-format +external-format+
                                     :reuse-address #t)))
           (unwind-protect
                (progn
                  (server.dribble "Setting socket ~A to be non-blocking" socket)
                  (setf (io.streams:fd-non-blocking socket) #t)
                  ;;(net.sockets:set-socket-option socket :receive-timeout :sec 1 :usec 0)
                  (setf (socket-of server) socket)
                  (setf socket-is-ok t)
                  (return))
             (when (and (not socket-is-ok)
                        socket)
               (close socket))))))
    (if (zerop (maximum-worker-count-of server))
        ;; run in single-threaded mode (mostly for profiling)
        (worker-loop server #f)
        (let ((ok nil))
          (unwind-protect
               (progn
                 (server.debug "Spawning the initial workers")
                 (iter (for n :from 0 :below initial-worker-count)
                       (make-worker server))
                 (setf (started-at-of server) (local-time:now))
                 (server.debug "Server successfully started")
                 (setf ok t))
            (unless ok
              (server.debug "Cleaning up after a failed server start")
              (shutdown-server server :force #t))))))
  server)

(defmethod shutdown-server ((server server) &key force &allow-other-keys)
  (setf (shutdown-initiated-p server) #t)
  (macrolet ((kill-thread-and-catch-error (thread)
               (once-only (thread)
                 `(block kill-worker
                   (handler-bind ((error (lambda (c)
                                           (warn "Error while killing ~S: ~A." ,thread c)
                                           (return-from kill-worker))))
                     (let ((os-thread (thread-of ,thread)))
                       (server.dribble "Killing thread ~A; os thread is ~A" ,thread os-thread)
                       (destroy-thread os-thread)))))))
    (flet ((close-socket ()
             (awhen (socket-of server)
               (setf (socket-of server) nil)
               (server.dribble "Closing socket ~A" it)
               (close it :abort force))))
      (server.dribble "Shutting down server ~A, force? ~A" server force)
      (bind ((threaded? (not (zerop (maximum-worker-count-of server)))))
        (if force
            (progn
              (when threaded?
                (iter (for worker :in-sequence (copy-seq (workers-of server)))
                      (kill-thread-and-catch-error worker)))
              (close-socket)
              (when threaded?
                (setf (fill-pointer (workers-of server)) 0)
                (setf (occupied-worker-count-of server) 0)))
            (progn
              (when threaded?
                (iter waiting-for-workers
                      (server.debug "Waiting for the workers of ~A to quit..." server)
                      (with-lock-held-on-server server
                        (when (zerop (length (workers-of server)))
                          (return-from waiting-for-workers)))
                      (sleep 1))
                (assert (zerop (occupied-worker-count-of server))))
              (close-socket)))))))

(def class* worker ()
  ((thread)))

(defun make-worker (server)
  (with-lock-held-on-server server
    (let ((worker (make-instance 'worker)))
      (setf (thread-of worker)
            (make-thread (lambda ()
                           (worker-loop server #t worker))
                         :name (format nil "http worker ~a" (length (workers-of server)))))
      (register-worker worker server)
      (server.info "Spawned new worker thread ~A" worker)
      worker)))

(defun register-worker (worker server)
  (with-lock-held-on-server server
    (vector-push-extend worker (workers-of server))))

(defun unregister-worker (worker server)
  (with-lock-held-on-server server
    (deletef (workers-of server) worker)))

(defun worker-loop (server &optional (threaded? #f) worker)
  (assert (or (not threaded?) worker))
  (with-lock-held-on-server server
    ;; wait until the startup procedure finished
    )
  (unwind-protect
       (restart-case
            (iter accepting
                  (with socket = (socket-of server))
                  (until (shutdown-initiated-p server))
                  (for (values readable writable) = (wait-until-fd-ready (fd-of socket) :input 1))
                  ;;(server.dribble "wait-until-fd-ready returned with readable ~S, writable ~S in thread ~A" readable writable (current-thread))
                  (until (shutdown-initiated-p server))
                  (unless readable
                    (next-iteration))
                  (for stream-socket = (accept-connection socket))
                  (when stream-socket
                    (flet ((serve-one-request ()
                             (unwind-protect
                                  (progn
                                    (server.dribble "Worker ~A is processing a request" worker)
                                    (with-lock-held-on-server server
                                      (incf (occupied-worker-count-of server))
                                      (when (and threaded?
                                                 (= (occupied-worker-count-of server)
                                                    (length (workers-of server)))
                                                 (< (length (workers-of server))
                                                    (maximum-worker-count-of server)))
                                        (server.info "All ~A worker threads are occupied, starting a new one" (length (workers-of server)))
                                        (make-worker server))
                                      (incf (processed-request-count-of server)))
                                    (bind ((*server* server)
                                           (*brokers* (list server))
                                           (*request* (read-request server stream-socket)))
                                      (loop
                                         (with-simple-restart (retry-handling "Try again handling this request")
                                           (funcall (handler-of server))
                                           (return)))
                                      (close-request *request*)))
                               (with-lock-held-on-server server
                                 (decf (occupied-worker-count-of server)))))
                           (handle-request-error (condition)
                             (server.error "Error while handling a server request in worker ~A on socket ~A: ~A" worker stream-socket condition)
                             (bind ((broker (when (boundp '*brokers*)
                                              (first *brokers*))))
                               (handle-toplevel-condition broker condition))
                             (server.error "Should not get here, removing this worker...")
                             (return-from accepting)))
                      (unwind-protect
                           (progn
                             (call-as-server-request-handler #'serve-one-request
                                                             stream-socket
                                                             :error-handler #'handle-request-error)
                             (server.dribble "Worker ~A finished processing a request" worker))
                        (close stream-socket)))))
        (remove-worker ()
          :report (lambda (stream)
                    (format stream "Stop and remove worker ~A" worker))
          (values)))
    (when worker
      (unregister-worker worker server))
    (server.dribble "Worker ~A is going away" worker)))

(defun call-as-server-request-handler (thunk network-stream &key (error-handler 'abort-server-request))
  (restart-case
      (let ((swank::*sldb-quit-restart* 'abort-server-request))
        (call-with-server-error-handler thunk network-stream error-handler))
    (abort-server-request ()
      :report "Abort processing this request by simply closing the network socket"
      (values))))

(def function abort-server-request (&optional (why nil why-p))
  (server.info "Gracefully aborting request~:[.~; because: ~A.~]" why-p why)
  (invoke-restart (find-restart 'abort-server-request)))

(defmethod read-request ((server server) stream)
  (let ((*request-content-length-limit* (request-content-length-limit-of server)))
    (read-http-request stream)))


;;;;;;;;;;;;;;;;;
;;; serving stuff

(def definer content-serving-function (name args (&key last-modified-at seconds-until-expires content-length)
                                            &body body)
  (bind (((:values body declarations documentation) (parse-body body :documentation #t))
         (%last-modified-at last-modified-at)
         (%seconds-until-expires seconds-until-expires)
         (%content-length content-length))
    (with-unique-names (response if-modified-since seconds-until-expires last-modified-at content-length)
      `(defun ,name ,args
         ,@(awhen documentation (list it))
         ,@declarations
         (bind ((,last-modified-at ,%last-modified-at)
                (,seconds-until-expires ,%seconds-until-expires)
                (,content-length ,%content-length)
                (,response *response*)
                (,if-modified-since (header-value *request* +header/if-modified-since+)))
           (when ,seconds-until-expires
             (if (<= ,seconds-until-expires 0)
                 (disallow-response-caching ,response)
                 ;; TODO check possible timezone issues with calling date:universal-time-to-http-date like this
                 (setf (header-value ,response +header/expires+)
                       (date:universal-time-to-http-date
                        (+ (get-universal-time) ,seconds-until-expires)))))
           (when ,last-modified-at
             (setf (header-value ,response +header/last-modified+)
                   (date:universal-time-to-http-date ,last-modified-at)))
           (setf (header-value ,response +header/date+)
                 (date:universal-time-to-http-date (get-universal-time)))
           (if (and ,last-modified-at
                    ,if-modified-since
                    (<= ,last-modified-at
                        (date:parse-time
                         ;; IE sends junk with the date (but sends it after a semicolon)
                         (subseq ,if-modified-since 0 (position #\; ,if-modified-since :test #'char=)))))
               (progn
                 (server.dribble ,(format nil "~A: Sending 304 not modified, and the headers only" name))
                 (setf (header-value ,response +header/status+) +http-not-modified+)
                 (setf (header-value ,response +header/content-length+) "0")
                 (send-headers ,response))
               (progn
                 (setf (header-value ,response +header/status+) +http-ok+)
                 (when ,content-length
                   (setf (header-value ,response +header/content-length+)
                         (princ-to-string ,content-length)))
                 ,@body)))))))

(def content-serving-function serve-sequence (sequence &key
                                                       (last-modified-at (get-universal-time))
                                                       (content-type "application/octet-stream")
                                                       content-disposition
                                                       (seconds-until-expires #.(* 60 60)))
  (:last-modified-at last-modified-at :seconds-until-expires seconds-until-expires)
  "Write SEQUENCE into the network stream. SEQUENCE may be a string or a byte vector. When it's a string it will be encoded using the (external-format-of *response*)."
  (bind ((response *response*)
         (bytes (if (stringp sequence)
                    (string-to-octets sequence :encoding (external-format-of response))
                    sequence)))
    (setf (header-value response +header/content-type+) content-type)
    (setf (header-value response +header/content-length+) (princ-to-string (length bytes)))
    (awhen content-disposition
      (setf (header-value response +header/content-disposition+) it))
    (send-headers response)
    (write-sequence bytes (network-stream-of *request*))))

(def content-serving-function serve-stream (stream &key
                                                   (last-modified-at (get-universal-time))
                                                   content-type
                                                   content-length
                                                   (content-disposition "attachment" content-disposition-p)
                                                   content-disposition-filename
                                                   content-disposition-size
                                                   (seconds-until-expires #.(* 24 60 60)))
  (:last-modified-at last-modified-at :seconds-until-expires seconds-until-expires)
  (bind ((response *response*))
    (awhen content-type
      (setf (header-value response +header/content-type+) it))
   (when (and (not content-length)
              (typep stream 'file-stream))
     (setf content-length (princ-to-string (file-length stream))))
   (unless (stringp content-length)
     (setf content-length (princ-to-string content-length)))
   (awhen content-length
     (setf (header-value response +header/content-length+) it))
   (unless content-disposition-p
     ;; this ugliness here is to give a chance for the caller to pass NIL directly
     ;; to disable the default content disposition parts.
     (when (and (not content-disposition-size)
                content-length)
       (setf content-disposition-size content-length))
     (awhen content-disposition-size
       (setf content-disposition (concatenate 'string content-disposition ";size=" it)))
     (awhen content-disposition-filename
       (setf content-disposition (concatenate 'string content-disposition ";filename=\"" it "\""))))
   (awhen content-disposition
     (setf (header-value response +header/content-disposition+) it))
   (send-headers response)
   (loop
      :with buffer = (make-array 8192 :element-type 'unsigned-byte)
      :with network-stream = (network-stream-of *request*)
      :for end-pos = (read-sequence buffer stream)
      :until (zerop end-pos)
      :do
      (write-sequence buffer network-stream :end end-pos))))

(def function serve-file (file-name &rest args &key
                                    (last-modified-at (file-write-date file-name))
                                    (content-type nil content-type-p)
                                    (for-download #f)
                                    (content-disposition-filename nil content-disposition-filename-p)
                                    content-disposition-size
                                    (encoding :utf-8)
                                    (seconds-until-expires #.(* 24 60 60))
                                    (signal-errors #t)
                                    &allow-other-keys)
  (bind ((network-stream-dirty? #f))
    (handler-bind ((serious-condition (lambda (error)
                                        (unless signal-errors
                                          (return-from serve-file (values #f error network-stream-dirty?))))))
      (with-open-file (file file-name :direction :input :element-type '(unsigned-byte 8))
        (unless for-download
          (unless content-type-p
            (setf content-type (or content-type
                                   (switch ((pathname-type file-name) :test #'string=)
                                     ("html" (content-type-for +html-mime-type+ encoding))
                                     ("css"  (content-type-for +css-mime-type+ encoding))
                                     (t (or (first (awhen (pathname-type file-name)
                                                     (mime-types-for-extension it)))
                                            (content-type-for +plain-text-mime-type+ encoding)))))))
          (unless content-disposition-filename-p
            (setf content-disposition-filename (concatenate 'string
                                                            (pathname-name file-name)
                                                            (awhen (pathname-type file-name)
                                                              (concatenate 'string "." it))))))
        (setf network-stream-dirty? #t)
        (apply 'serve-stream
               file
               :last-modified-at last-modified-at
               :content-type content-type
               :content-disposition-filename content-disposition-filename
               :content-disposition-size content-disposition-size
               :seconds-until-expires seconds-until-expires
               (append
                (unless for-download
                  (list :content-disposition nil))
                args))
        (values #t nil #f)))))
