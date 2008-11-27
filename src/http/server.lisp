;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (class* e) request-counter-mixin ()
  ((processed-request-count 0)))

(def class* server-listen-entry ()
  ((host)
   (port)
   (ssl-certificate nil)
   (socket nil)))

(def (class* e) server (request-counter-mixin)
  ((admin-email-address nil)
   (listen-entries ())
   (connection-multiplexer nil)
   (handler :type (or symbol function))
   (request-content-length-limit *request-content-length-limit*)
   (lock (make-recursive-lock "WUI server lock"))
   (shutdown-initiated #f :type boolean)
   (workers (make-adjustable-vector 16))
   (maximum-worker-count 16 :export :accessor)
   (occupied-worker-count 0)
   (started-at nil)
   (timer nil)
   (profile-request-processing #f :type boolean :export :accessor)))

(def constructor (server (listen-entries nil listen-entries-p) host port ssl-certificate)
  (when (and (or host port ssl-certificate)
             listen-entries-p)
    (error "Either use the :HOST, :PORT and :SSL-CERTIFICATE arguments or the full :LISTEN-ENTRIES version, but don't mix them"))
  (cond
    (listen-entries-p
     (setf (listen-entries-of -self-) (mapcar (lambda (listen-entry)
                                                (etypecase listen-entry
                                                  (server-listen-entry listen-entry)
                                                  (cons (apply #'make-instance 'server-listen-entry listen-entry))))
                                              listen-entries)))
    (host
     (setf (listen-entries-of -self-) (list (make-instance 'server-listen-entry
                                                           :host host
                                                           :port (or port 80)
                                                           :ssl-certificate ssl-certificate))))))

(def function print-object/server (server)
  (write-string "listen: ")
  (iter (for listen-entry :in (listen-entries-of server))
        (unless (first-time-p)
          (write-string ", "))
        (princ (host-of listen-entry))
        (write-string "/")
        (princ (port-of listen-entry))))

(def print-object server
  (print-object/server -self-))

(def (function e) is-server-running? (server)
  (not (null (started-at-of server))))

(def (with-macro* e) with-lock-held-on-server (server)
  (multiple-value-prog1
      (progn
        (threads.dribble "Entering with-lock-held-on-server for server ~S in thread ~S" server (current-thread))
        (with-recursive-lock-held ((lock-of server))
          (-body-)))
    (threads.dribble "Leaving with-lock-held-on-server for server ~S in thread ~S" server (current-thread))))

(def (generic e) startup-server (server &key &allow-other-keys))
(def (generic e) shutdown-server (server &key &allow-other-keys))

(defmethod startup-server ((server server) &key (initial-worker-count 2) &allow-other-keys)
  (server.debug "STARTUP-SERVER of ~A" server)
  (assert (listen-entries-of server))
  (setf (shutdown-initiated-p server) #f)
  (restart-case
      (bind ((swank::*sldb-quit-restart* (find-restart 'abort)))
        (with-lock-held-on-server (server)
          (bind ((listen-entries (listen-entries-of server))
                 (mux (make-instance 'epoll-multiplexer)))
            (unwind-protect-case ()
                (progn
                  (assert listen-entries)
                  (assert (not (find-if (complement #'null) listen-entries :key 'socket-of)))
                  (dolist (listen-entry listen-entries)
                    (bind ((host (host-of listen-entry))
                           (port (port-of listen-entry)))
                      (loop :named binding :do
                         (with-simple-restart (retry "Try opening the socket again on host ~S port ~S" host port)
                           (server.debug "Binding socket to host ~A, port ~A" host port)
                           (bind ((socket (make-socket :connect :passive
                                                       :local-host host
                                                       :local-port port
                                                       :external-format +external-format+
                                                       :reuse-address #t))
                                  (fd (fd-of socket)))
                             (unwind-protect-case ()
                                 (progn
                                   (assert fd)
                                   (server.dribble "Setting socket ~A to be non-blocking" socket)
                                   (setf (io.streams:fd-non-blocking socket) #t)
                                   ;;(net.sockets:set-socket-option socket :receive-timeout :sec 1 :usec 0)
                                   (server.debug "Adding socket ~A, fd ~A to the accept multiplexer" socket fd)
                                   (iomux::monitor-fd mux (aprog1
                                                              (iomux::make-fd-entry fd)
                                                             ;; KLUDGE to make the multiplexer do what we want
                                                            (setf (iomux::fd-entry-read-handler it)
                                                                  (iomux::make-fd-handler fd :read (lambda (&rest args)
                                                                                                     (declare (ignore args))
                                                                                                     (error "BUG"))
                                                                                          nil))))
                                   (setf (socket-of listen-entry) socket))
                               (:abort (close socket)))
                             (return-from binding))))))
                  (setf (connection-multiplexer-of server) mux))
              (:abort (dolist (listen-entry (listen-entries-of server))
                        (awhen (socket-of listen-entry)
                          (close it)
                          (setf (socket-of listen-entry) nil)))
                      (io.multiplex::close-multiplexer mux)
                      (setf (connection-multiplexer-of server) nil))))
          (if (zerop (maximum-worker-count-of server))
              (unwind-protect
                   ;; run in single-threaded mode (for debugging and profiling)
                   (progn
                     (server.info "Starting server in the current thread, use C-c C-c to break out...")
                     (worker-loop server #f))
                (shutdown-server server))
              (unwind-protect-case ()
                  (progn
                    (server.debug "Setting up the timer")
                    (assert (null (timer-of server)))
                    (setf (timer-of server) (make-instance 'timer))
                    (server.debug "Spawning the initial workers")
                    (iter (for n :from 0 :below initial-worker-count)
                          (make-worker server))
                    (setf (started-at-of server) (local-time:now))
                    (server.debug "Server successfully started"))
                (:abort
                 (server.debug "Cleaning up after a failed server start")
                 (shutdown-server server :force #t)))))
        server)
    (abort ()
      :report (lambda (stream)
                (format stream "Give up starting server ~A" server))
      (values))))

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
    (flet ((close-sockets ()
             (dolist (listen-entry (listen-entries-of server))
               (awhen (socket-of listen-entry)
                 (setf (socket-of listen-entry) nil)
                 (server.dribble "Closing socket ~A" it)
                 (close it :abort force)))
             (io.multiplex::close-multiplexer (connection-multiplexer-of server))
             (setf (connection-multiplexer-of server) nil)
             (setf (started-at-of server) nil)))
      (server.dribble "Shutting down server ~A, force? ~A" server force)
      (bind ((threaded? (not (zerop (maximum-worker-count-of server)))))
        (when threaded?
          (shutdown-timer (timer-of server) :wait (not force))
          (setf (timer-of server) nil))
        (if force
            (progn
              (when threaded?
                (iter (for worker :in-sequence (copy-seq (workers-of server)))
                      (kill-thread-and-catch-error worker)))
              (close-sockets)
              (when threaded?
                (setf (fill-pointer (workers-of server)) 0)
                (setf (occupied-worker-count-of server) 0)))
            (progn
              (when threaded?
                (iter waiting-for-workers
                      (server.debug "Waiting for the workers of ~A to quit..." server)
                      (with-lock-held-on-server (server)
                        (when (zerop (length (workers-of server)))
                          (return-from waiting-for-workers)))
                      (sleep 1))
                (assert (zerop (occupied-worker-count-of server))))
              (close-sockets)))))))

(def class* worker ()
  ((thread)))

(defun make-worker (server)
  (with-lock-held-on-server (server)
    (let ((worker (make-instance 'worker)))
      (setf (thread-of worker)
            (make-thread (lambda ()
                           (worker-loop server #t worker))
                         :name (format nil "http worker ~a" (length (workers-of server)))))
      (register-worker worker server)
      (server.info "Spawned new worker thread ~A" worker)
      worker)))

(defun register-worker (worker server)
  (with-lock-held-on-server (server)
    (vector-push-extend worker (workers-of server))))

(defun unregister-worker (worker server)
  (with-lock-held-on-server (server)
    (deletef (workers-of server) worker)))

(defun worker-loop (server &optional (threaded? #f) worker)
  (assert (or (not threaded?) worker))
  (with-lock-held-on-server (server)
    ;; wait until the startup procedure finished
    )
  (flet ((body ()
           (bind ((mux (connection-multiplexer-of server))
                  (listen-entries (listen-entries-of server)))
             (assert mux)
             (iter
               (until (shutdown-initiated-p server))
               ;; (server.dribble "Acceptor multiplexer ticked")
               (iter (for (fd event-types) :in (iomux::harvest-events mux 1))
                     (server.dribble "Acceptor multiplexer returned with fd ~A, events ~S" fd event-types)
                     (until (shutdown-initiated-p server))
                     (when (member :read event-types :test #'eq)
                       (server.debug "Acceptor multiplexer handed a :read event for fd ~A" fd)
                       (bind ((listen-entry (aprog1
                                                (find fd listen-entries :key [fd-of (socket-of !1)])
                                              (assert it () "listen-entry not found for fd?!")))
                              (socket (socket-of listen-entry)))
                         (iter (for stream-socket = (accept-connection socket :wait #f))
                               (while (and stream-socket
                                           (not (shutdown-initiated-p server))))
                               (worker-loop/serve-one-request threaded? server worker stream-socket)))))))
           (values)))
    (if threaded?
        (unwind-protect
             (restart-case
                 (body)
               (remove-worker ()
                 :report (lambda (stream)
                           (format stream "Stop and remove worker ~A" worker))
                 (values)))
          (when worker
            (unregister-worker worker server))
          (server.dribble "Worker ~A is going away" worker))
        (body))))

(def function worker-loop/serve-one-request (threaded? server worker stream-socket)
  (flet ((serve-one-request ()
           (unwind-protect
                (progn
                  (server.dribble "Worker ~A is processing a request" worker)
                  (with-lock-held-on-server (server)
                    (incf (occupied-worker-count-of server))
                    (when (and threaded?
                               (= (occupied-worker-count-of server)
                                  (length (workers-of server)))
                               (< (length (workers-of server))
                                  (maximum-worker-count-of server)))
                      (server.info "All ~A worker threads are occupied, starting a new one" (length (workers-of server)))
                      (make-worker server))
                    (incf (processed-request-count-of server)))
                  (setf *request* (read-request server stream-socket))
                  (loop
                     (with-simple-restart (retry-handling-request "Try again handling this request")
                       (when (and *response*
                                  (headers-are-sent-p *response*))
                         (cerror "Continue" "Some data was already written to the network stream, so restarting the request handling will probably not result in what you would expect."))
                       (setf *response* nil)
                       (funcall (handler-of server))
                       (return)))
                  (close-request *request*))
             (with-lock-held-on-server (server)
               (decf (occupied-worker-count-of server)))))
         (handle-request-error (condition)
           (server.error "Error while handling a server request in worker ~A on socket ~A: ~A" worker stream-socket condition)
           (bind ((broker (when (boundp '*brokers*)
                            (first *brokers*))))
             ;; no need to handle errors here, see CALL-WITH-SERVER-ERROR-HANDLER.
             (handle-toplevel-condition broker condition))
           (server.dribble "HANDLE-TOPLEVEL-CONDITION returned, worker continues...")))
    (unwind-protect
         (with-thread-name (concatenate-string " / serving request "
                                               (integer-to-string (processed-request-count-of server)))
           (debug-only (assert (notany #'boundp '(*server* *brokers* *request* *response*))))
           (bind ((*server* server)
                  (*brokers* (list server))
                  (*request* nil)
                  (*response* nil))
             (call-as-server-request-handler #'serve-one-request
                                             stream-socket
                                             :error-handler #'handle-request-error))
           (server.dribble "Worker ~A finished processing a request" worker))
      (block nil
        (call-with-server-error-handler (lambda ()
                                          (close stream-socket))
                                        stream-socket
                                        (lambda (error)
                                          (server.warn "Failed to close the socket stream while unwinding in SERVE-ONE-REQUEST due to ~A" error)
                                          (return)))))))

(def function store-response (response)
  (assert (boundp '*response*))
  (unless (typep response 'do-nothing-response)
    (assert (or (null *response*)
                (eq *response* response)))
    (setf *response* response)))

(def (function e) invoke-retry-handling-request-restart ()
  (invoke-restart (find-restart 'retry-handling-request)))

(defun call-as-server-request-handler (thunk network-stream &key (error-handler 'abort-server-request))
  (restart-case
      (let ((swank::*sldb-quit-restart* (find-restart 'abort-server-request)))
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

(def method handle-request :around ((server server) (request request))
  (bind ((start-time (get-monotonic-time))
         (start-bytes-allocated (get-bytes-allocated))
         (remote-host (remote-host-of request))
         (raw-uri (raw-uri-of request)))
    (http.info "Handling request from ~S for ~S, method ~S" remote-host raw-uri (http-method-of request))
    (multiple-value-prog1
        (if (profile-request-processing-p server)
            (call-with-profiling #'call-next-method)
            (call-next-method))
      (bind ((seconds (- (get-monotonic-time) start-time))
             (bytes-allocated (- (get-bytes-allocated) start-bytes-allocated)))
        (http.info "Handled request in ~,3f secs, ~,3f MB allocated (request came from ~S for ~S)"
                   seconds (/ bytes-allocated 1024 1024) remote-host raw-uri)))))

(def (function e) is-request-still-valid? ()
  (iolib:socket-connected-p (network-stream-of *request*)))

(def (function e) abort-request-unless-still-valid ()
  (unless (is-request-still-valid?)
    (abort-server-request "The request's socket is not connected anymore")))


;;;;;;;;;;;;;;;;;
;;; serving stuff

(def definer content-serving-function (name args (&key headers cookies (stream '(network-stream-of *request*))
                                                       last-modified-at seconds-until-expires content-length)
                                            &body body)
  (bind (((:values body declarations documentation) (parse-body body :documentation #t))
         (%last-modified-at last-modified-at)
         (%seconds-until-expires seconds-until-expires)
         (%content-length content-length))
    (with-unique-names (if-modified-since if-modified-since-value seconds-until-expires last-modified-at content-length)
      `(defun ,name ,args
         ,@(awhen documentation (list it))
         ,@declarations
         (bind ((,last-modified-at ,%last-modified-at)
                (,seconds-until-expires ,%seconds-until-expires)
                (,content-length ,%content-length)
                (,if-modified-since (header-value *request* +header/if-modified-since+))
                ;; TODO get rid of this final net.telent.date dependency
                (,if-modified-since-value (when ,if-modified-since
                                            (local-time:universal-to-timestamp
                                             (net.telent.date:parse-time
                                              ;; IE sends junk with the date (but sends it after a semicolon)
                                              (subseq ,if-modified-since 0 (position #\; ,if-modified-since :test #'char=)))))))
           (when ,seconds-until-expires
             (if (<= ,seconds-until-expires 0)
                 (disallow-response-caching-in-header-alist ,headers)
                 (setf (header-alist-value ,headers +header/expires+)
                       (local-time:to-http-timestring
                        (local-time:adjust-timestamp (local-time:now) (offset :sec ,seconds-until-expires))))))
           (when ,last-modified-at
             (setf (header-alist-value ,headers +header/last-modified+)
                   (local-time:to-http-timestring ,last-modified-at)))
           (setf (header-alist-value ,headers +header/date+) (local-time:to-http-timestring (local-time:now)))
           (server.dribble "~A: if-modified-since is ~S, last-modified-at is ~A, if-modified-since-value is ~A" ',name ,if-modified-since ,last-modified-at ,if-modified-since-value)
           (if (and ,last-modified-at
                    ,if-modified-since
                    (local-time:timestamp<= ,last-modified-at ,if-modified-since-value))
               (progn
                 (server.debug "~A: Sending 304 not modified. if-modified-since is ~S, last-modified-at is ~S" ',name ,if-modified-since ,last-modified-at)
                 (setf (header-alist-value ,headers +header/status+) +http-not-modified+)
                 (setf (header-alist-value ,headers +header/content-length+) "0")
                 (send-http-headers ,headers ,cookies :stream ,stream))
               (progn
                 (setf (header-alist-value ,headers +header/status+) +http-ok+)
                 (when ,content-length
                   (setf (header-alist-value ,headers +header/content-length+)
                         (integer-to-string ,content-length)))
                 ,@body)))))))

(def content-serving-function serve-sequence (sequence &key
                                                       (last-modified-at (local-time:now))
                                                       (content-type +octet-stream-mime-type+)
                                                       headers
                                                       cookies
                                                       content-disposition
                                                       (stream (network-stream-of *request*))
                                                       (seconds-until-expires #.(* 60 60)))
    (:last-modified-at last-modified-at
     :seconds-until-expires seconds-until-expires
     :headers headers
     :cookies cookies
     :stream stream)
  "Write SEQUENCE into the network stream. SEQUENCE may be a string or a byte vector. When it's a string it will be encoded using the current external-format of the network stream."
  (bind ((bytes (if (stringp sequence)
                    (string-to-octets sequence :encoding (external-format-of stream))
                    sequence)))
    (setf (header-alist-value headers +header/content-type+) content-type)
    (setf (header-alist-value headers +header/content-length+) (integer-to-string (length bytes)))
    (awhen content-disposition
      (setf (header-alist-value headers +header/content-disposition+) it))
    (send-http-headers headers cookies :stream stream)
    (write-sequence bytes stream)))

(def content-serving-function serve-stream (input-stream &key
                                                         (last-modified-at (local-time:now))
                                                         content-type
                                                         content-length
                                                         (content-disposition "attachment" content-disposition-p)
                                                         headers
                                                         cookies
                                                         content-disposition-filename
                                                         content-disposition-size
                                                         (stream (network-stream-of *request*))
                                                         (seconds-until-expires #.(* 24 60 60)))
    (:last-modified-at last-modified-at
     :seconds-until-expires seconds-until-expires
     :headers headers
     :cookies cookies
     :stream stream)
  (awhen content-type
    (setf (header-alist-value headers +header/content-type+) it))
  (when (and (not content-length)
             (typep input-stream 'file-stream))
    (setf content-length (integer-to-string (file-length input-stream))))
  (unless (stringp content-length)
    (setf content-length (integer-to-string content-length)))
  (awhen content-length
    (setf (header-alist-value headers +header/content-length+) it))
  (unless content-disposition-p
    ;; this ugliness here is to give a chance for the caller to pass NIL directly
    ;; to disable the default content disposition parts.
    (when (and (not content-disposition-size)
               content-length)
      (setf content-disposition-size content-length))
    (awhen content-disposition-size
      (setf content-disposition (concatenate 'string content-disposition ";size=" it))) ;
    (awhen content-disposition-filename
      (setf content-disposition (concatenate 'string content-disposition ";filename=\"" it "\""))))
  (awhen content-disposition
    (setf (header-alist-value headers +header/content-disposition+) it))
  (send-http-headers headers cookies)
  (loop
     :with buffer = (make-array 8192 :element-type 'unsigned-byte)
     :for end-pos = (read-sequence buffer input-stream)
     :until (zerop end-pos)
     :do
     (write-sequence buffer stream :end end-pos)))

(def function serve-file (file-name &rest args &key
                                    (last-modified-at (local-time:universal-to-timestamp
                                                       (file-write-date file-name)))
                                    (content-type nil content-type-p)
                                    (for-download #f)
                                    (content-disposition-filename nil content-disposition-filename-p)
                                    headers
                                    cookies
                                    (stream (network-stream-of *request*))
                                    content-disposition-size
                                    (encoding :utf-8)
                                    (seconds-until-expires #.(* 12 60 60))
                                    (signal-errors #t)
                                    &allow-other-keys)
  (remove-from-plistf args :signal-errors)
  (bind ((network-stream-dirty? #f))
    (handler-bind ((serious-condition (lambda (error)
                                        (unless signal-errors
                                          (return-from serve-file (values #f error network-stream-dirty?))))))
      (with-open-file (file file-name :direction :input :element-type '(unsigned-byte 8))
        (unless for-download
          (unless content-type-p
            (setf content-type (or content-type
                                   (switch ((pathname-type file-name) :test #'string=)
                                     ;; this special-casing is an optimization to cons less
                                     ("html" (content-type-for +html-mime-type+ encoding))
                                     ("xml" (content-type-for +xml-mime-type+ encoding))
                                     ("css"  (content-type-for +css-mime-type+ encoding))
                                     ("csv"  (content-type-for +csv-mime-type+ encoding))
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
               :headers headers
               :cookies cookies
               :stream stream
               (append
                (unless for-download
                  (list :content-disposition nil))
                args))
        (values #t nil #f)))))
