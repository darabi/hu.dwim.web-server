;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) handle-request (thing request))

(def (class* e) request-counter-mixin ()
  ((processed-request-count 0 :export :accessor)))

(def class* server-listen-entry ()
  ((host)
   (port)
   (ssl-certificate nil)
   (socket nil)))

(def print-object (server-listen-entry :identity #f :type #f)
  (princ (iolib:address-to-string (host-of -self-)))
  (write-string "/")
  (princ (port-of -self-)))

(def (class* e) server (request-counter-mixin)
  ((admin-email-address nil)
   (gracefully-aborted-request-count 0 :export :accessor)
   (failed-request-count 0 :export :accessor)
   (client-connection-reset-count 0 :export :accessor)
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
   (profile-request-processing? #f :type boolean :export :accessor)))

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
        (princ (iolib:address-to-string (host-of listen-entry)))
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

(def method startup-server ((server server) &key (initial-worker-count 2) &allow-other-keys)
  (server.debug "STARTUP-SERVER of ~A" server)
  (assert (listen-entries-of server))
  (setf (shutdown-initiated-p server) #f)
  (restart-case
      (bind ((swank::*sldb-quit-restart* (find-restart 'abort)))
        (with-lock-held-on-server (server) ; the started workers are waiting until this lock is released
          (bind ((listen-entries (listen-entries-of server))
                 (mux (make-instance 'iolib:epoll-multiplexer)))
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
                           (bind ((socket (iolib:make-socket :connect :passive
                                                             :local-host host
                                                             :local-port port
                                                             :external-format +external-format+
                                                             :reuse-address #t))
                                  (fd (iolib:fd-of socket)))
                             (unwind-protect-case ()
                                 (progn
                                   (assert fd)
                                   (server.dribble "Setting socket ~A to be non-blocking" socket)
                                   (setf (iolib.streams:fd-non-blocking socket) #t)
                                   ;;(setf (iolib:socket-option socket :receive-timeout) 1)
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
                      (iolib.multiplex::close-multiplexer mux)
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

(def method shutdown-server ((server server) &key force &allow-other-keys)
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
             (iolib.multiplex::close-multiplexer (connection-multiplexer-of server))
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
                        (bind ((worker-count (length (workers-of server))))
                          (server.dribble "Polling workers to quit, worker-count is ~A" worker-count)
                          (when (zerop worker-count)
                            (return-from waiting-for-workers))))
                      (sleep 1))
                (assert (zerop (occupied-worker-count-of server))))
              (close-sockets)))))))

(def class* worker ()
  ((thread)))

(def function make-worker (server)
  (with-lock-held-on-server (server)
    (let ((worker (make-instance 'worker)))
      (setf (thread-of worker)
            (make-thread (lambda ()
                           (worker-loop server #t worker))
                         :name (format nil "http worker ~a" (length (workers-of server)))))
      (register-worker worker server)
      (server.info "Spawned new worker thread ~A" worker)
      worker)))

(def function register-worker (worker server)
  (with-lock-held-on-server (server)
    (vector-push-extend worker (workers-of server))))

(def function unregister-worker (worker server)
  (with-lock-held-on-server (server)
    (deletef (workers-of server) worker)))

(def function worker-loop (server &optional (threaded? #f) worker)
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
                                                (find fd listen-entries :key [iolib:fd-of (socket-of !1)])
                                              (unless it
                                                (error "listen-entry not found for fd ~A?!" fd))))
                              (socket (socket-of listen-entry)))
                         (iter (for stream-socket = (iolib:accept-connection socket :wait #f))
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
          (server.dribble "Worker ~A is about to leave. There are ~A workers currently." worker (length (workers-of server)))
          (with-lock-held-on-server (server)
            (when worker
              (unregister-worker worker server))
            (when (and (not (shutdown-initiated-p server))
                       (zerop (length (workers-of server))))
              (make-worker server))))
        (body))))

(def function worker-loop/serve-one-request (threaded? server worker stream-socket)
  (flet ((serve-one-request ()
           (unwind-protect
                (progn
                  (server.dribble "Worker ~A is processing a request" worker)
                  (setf (iolib:socket-option stream-socket :receive-timeout) 5) ;; TODO is this a constant or depends on server network load?
                  (setf *request-remote-host* (iolib:remote-host stream-socket))
                  (with-lock-held-on-server (server)
                    (incf (occupied-worker-count-of server))
                    (bind ((worker-count (length (workers-of server))))
                      (when (and threaded?
                                 (= (occupied-worker-count-of server)
                                    worker-count))
                        (if (< worker-count
                               (maximum-worker-count-of server))
                            (progn
                              (server.info "All ~A worker threads are occupied, starting a new one" worker-count)
                              (make-worker server))
                            (server.warn "All ~A worker threads are occupied, and can't start new workers due to already at maximum-worker-count" worker-count))))
                    (setf *request-id* (incf (processed-request-count-of server))))
                  (with-thread-name (concatenate-string " / serving request " (integer-to-string *request-id*))
                    (setf *request* (read-request server stream-socket))
                    (loop
                       (with-simple-restart (retry-handling-request "Try again handling this request")
                         (when (and *response*
                                    (headers-are-sent-p *response*))
                           (cerror "Continue" "Some data was already written to the network stream, so restarting the request handling will probably not result in what you would expect."))
                         (setf *response* nil)
                         (funcall (handler-of server))
                         (return)))
                    (close-request *request*)))
             (with-lock-held-on-server (server)
               (decf (occupied-worker-count-of server)))))
         (handle-request-error (condition)
           (incf (failed-request-count-of server))
           (unless (typep condition 'access-denied-error)
             (server.error "Error while handling a server request in worker ~A on socket ~A: ~A" worker stream-socket condition))
           (bind ((broker (when (boundp '*brokers*)
                            (first *brokers*))))
             ;; no need to handle (nested) errors here, see CALL-WITH-SERVER-ERROR-HANDLER.
             (handle-toplevel-condition condition broker))
           (server.dribble "HANDLE-TOPLEVEL-CONDITION returned, worker continues...")))
    (debug-only (assert (notany #'boundp '(*server* *brokers* *request* *response* *request-remote-host* *request-id*))))
    (bind ((*server* server)
           (*brokers* (list server))
           (*request* nil)
           (*response* nil)
           (*request-remote-host* nil)
           (*request-id* nil))
      (with-error-log-decorator (special-variable-printing-error-log-decorator *request-remote-host* *request-id*)
        (restart-case
            (unwind-protect-case (interrupted)
                (bind ((swank::*sldb-quit-restart* (find-restart 'abort-server-request)))
                  (call-with-server-error-handler #'serve-one-request
                                                  stream-socket
                                                  #'handle-request-error)
                  (server.dribble "Worker ~A finished processing a request, will close the socket now" worker))
              (:always
               (block nil
                 (call-with-server-error-handler (lambda ()
                                                   (server.dribble "Closing the socket")
                                                   (close stream-socket))
                                                 stream-socket
                                                 (lambda (error)
                                                   (log-error-with-backtrace error :message (list "Failed to close the socket stream in SERVE-ONE-REQUEST while ~A the UNWIND-PROTECT block." (if interrupted "unwinding" "normally exiting")))
                                                   (return))))))
          (abort-server-request ()
            :report (lambda (stream)
                      (format stream "~@<Abort processing request ~A by simply closing the network socket~@:>" *request-id*))
            (values)))))))

(def function store-response (response)
  (assert (boundp '*response*))
  (unless (typep response 'do-nothing-response)
    (assert (or (null *response*)
                (eq *response* response)))
    (setf *response* response)))

(def (function e) invoke-retry-handling-request-restart ()
  (invoke-restart (find-restart 'retry-handling-request)))

(def function abort-server-request (&optional (why nil why-p))
  (server.info "Gracefully aborting request coming from ~S for ~S~:[.~; because: ~A.~]" *request-remote-host* (when *request* (raw-uri-of *request*)) why-p why)
  (typecase why
    (iolib:socket-connection-reset-error (incf (client-connection-reset-count-of *server*))))
  (invoke-restart (find-restart 'abort-server-request)))

(def method read-request ((server server) stream)
  (let ((*request-content-length-limit* (request-content-length-limit-of server)))
    (read-http-request stream)))

(def method handle-request :around ((server server) (request request))
  (bind ((start-time (get-monotonic-time))
         (start-bytes-allocated (get-bytes-allocated))
         (raw-uri (raw-uri-of request)))
    (http.info "Handling request ~S from ~S for ~S, method ~S" *request-id* *request-remote-host* raw-uri (http-method-of request))
    (multiple-value-prog1
        (if (profile-request-processing? server)
            (call-with-profiling #'call-next-method)
            (call-next-method))
      (bind ((seconds (- (get-monotonic-time) start-time))
             (bytes-allocated (- (get-bytes-allocated) start-bytes-allocated)))
        (http.info "Handled request ~S in ~,3f secs, ~,3f MB allocated (request came from ~S for ~S)"
                   *request-id* seconds (/ bytes-allocated 1024 1024) *request-remote-host* raw-uri)))))

(def (function e) is-request-still-valid? ()
  (iolib:socket-connected-p (client-stream-of *request*)))

(def (function e) abort-request-unless-still-valid ()
  (unless (is-request-still-valid?)
    (abort-server-request "The request's socket is not connected anymore")))


;;;;;;;;;;;;;;;;;
;;; serving stuff

(def definer content-serving-function (name args (&key headers cookies (stream '(client-stream-of *request*))
                                                       last-modified-at seconds-until-expires content-length)
                                            &body body)
  (bind (((:values body declarations documentation) (parse-body body :documentation #t))
         (%last-modified-at last-modified-at)
         (%seconds-until-expires seconds-until-expires)
         (%content-length content-length))
    (with-unique-names (if-modified-since if-modified-since-value seconds-until-expires last-modified-at content-length)
      `(def function ,name ,args
         ,@(awhen documentation (list it))
         ,@declarations
         (setf ,headers (copy-alist ,headers))
         (bind ((,last-modified-at ,%last-modified-at)
                (,seconds-until-expires ,%seconds-until-expires)
                (,content-length ,%content-length)
                (,if-modified-since (header-value *request* +header/if-modified-since+))
                ;; TODO get rid of this final net.telent.date dependency
                (,if-modified-since-value (when ,if-modified-since
                                            (bind ((http-date-string (subseq ,if-modified-since 0 (position #\; ,if-modified-since :test #'char=))) ; IE sends junk with the date (but sends it after a semicolon)
                                                   (universal (net.telent.date:parse-time http-date-string)))
                                              (if universal
                                                  (local-time:universal-to-timestamp universal)
                                                  (server.error "Failed to parse If-Modified-Since header value ~S" ,if-modified-since))))))
           (when ,seconds-until-expires
             (if (<= ,seconds-until-expires 0)
                 (disallow-response-caching-in-header-alist ,headers)
                 (progn
                   (setf (header-alist-value ,headers +header/expires+)
                         (local-time:to-http-timestring
                          (local-time:adjust-timestamp (local-time:now) (offset :sec ,seconds-until-expires))))
                   (setf (header-alist-value ,headers +header/cache-control+) (concatenate-string "max-age=" (integer-to-string ,seconds-until-expires))))))
           (when ,last-modified-at
             (setf (header-alist-value ,headers +header/last-modified+)
                   (local-time:to-http-timestring ,last-modified-at)))
           (setf (header-alist-value ,headers +header/date+) (local-time:to-http-timestring (local-time:now)))
           (server.dribble "~A: if-modified-since is ~S, last-modified-at is ~A, if-modified-since-value is ~A" ',name ,if-modified-since ,last-modified-at ,if-modified-since-value)
           (if (and ,last-modified-at
                    ,if-modified-since-value
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

(def content-serving-function serve-sequence (body &key
                                                   (last-modified-at (local-time:now))
                                                   content-type
                                                   content-encoding
                                                   headers
                                                   cookies
                                                   content-disposition
                                                   (stream (client-stream-of *request*))
                                                   (encoding (encoding-name-of (iolib:external-format-of stream)))
                                                   seconds-until-expires)
    (:last-modified-at last-modified-at
     :seconds-until-expires seconds-until-expires
     :headers headers
     :cookies cookies
     :stream stream)
  "Write SEQUENCE into the network stream. SEQUENCE may be a string or a byte vector. When it's a string it will be encoded using the current external-format of the network stream."
  (assert (or (null content-encoding)
              (string= content-encoding +content-encoding/deflate+)))
  (check-type body (or list (vector (unsigned-byte 8) *) string))
  (server.debug "SERVE-SEQUENCE: body type: ~A, length: ~A, content-encoding: ~A" (type-of body) (length body) content-encoding)
  (with-thread-name " / SERVE-SEQUENCE"
    (bind ((length 0)
           (body (iter (for piece :in (ensure-list body))
                       (when (stringp piece)
                         (setf piece (string-to-octets piece :encoding encoding)))
                       (incf length (length piece))
                       (collect piece))))
      (awhen content-type
        (setf (header-alist-value headers +header/content-type+) it))
      (awhen content-disposition
        (setf (header-alist-value headers +header/content-disposition+) it))
      (if content-encoding
          (switch (content-encoding :test #'equal)
            (+content-encoding/deflate+
             (setf (header-alist-value headers +header/content-encoding+) +content-encoding/deflate+)
             (bind ((bytes (if (and (length= 1 body)
                                    (typep (first body) 'simple-ub8-vector))
                               (first body)
                               (aprog1
                                   (make-array length :element-type '(unsigned-byte 8))
                                 (iter (for piece :in body)
                                       (for previous-piece :previous piece)
                                       (for start :first 0 :then (+ start (length previous-piece)))
                                       (replace it piece :start1 start))))))
               (bind ((compressed-bytes (hu.dwim.wui.zlib:allocate-compress-buffer bytes))
                      (compressed-bytes-length (hu.dwim.wui.zlib:compress bytes compressed-bytes)))
                 (server.debug "SERVE-SEQUENCE: compressed response, original-size ~A, compressed-size ~A, ratio: ~,3F" length compressed-bytes-length (/ compressed-bytes-length length))
                 (setf (header-alist-value headers +header/content-length+) (integer-to-string compressed-bytes-length))
                 (send-http-headers headers cookies :stream stream)
                 (write-sequence compressed-bytes stream :start 0 :end compressed-bytes-length)))))
          (progn
            (setf (header-alist-value headers +header/content-length+) (integer-to-string length))
            (send-http-headers headers cookies :stream stream)
            (dolist (piece body)
              (write-sequence piece stream)))))))

(def content-serving-function serve-stream (input-stream &key
                                                         (last-modified-at (local-time:now))
                                                         content-type
                                                         content-length
                                                         (content-disposition "attachment" content-disposition-p)
                                                         headers
                                                         cookies
                                                         content-disposition-filename
                                                         content-disposition-size
                                                         (stream (client-stream-of *request*))
                                                         (seconds-until-expires #.(* 24 60 60)))
    (:last-modified-at last-modified-at
     :seconds-until-expires seconds-until-expires
     :headers headers
     :cookies cookies
     :stream stream)
  (with-thread-name " / SERVE-STREAM"
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
    ;; TODO set socket buffering option?
    (send-http-headers headers cookies)
    (server.debug "SERVE-STREAM starts to copy input stream ~A to the network stream ~A" input-stream stream)
    (loop
       :with buffer = (make-array 8192 :element-type '(unsigned-byte 8))
       :for end-pos = (read-sequence buffer input-stream)
       :until (zerop end-pos)
       :do
       (progn
         (server.dribble "SERVE-STREAM will write ~A bytes to the network stream ~A" end-pos stream)
         (write-sequence buffer stream :end end-pos)))
    (server.debug "SERVE-STREAM finished copying input stream ~A to the network stream ~A" input-stream stream)))

(def function serve-file (file-name &rest args &key
                                    (last-modified-at (local-time:universal-to-timestamp
                                                       (file-write-date file-name)))
                                    (content-type nil content-type-p)
                                    (for-download #f)
                                    (content-disposition-filename nil content-disposition-filename-p)
                                    headers
                                    cookies
                                    (stream (client-stream-of *request*))
                                    content-disposition-size
                                    (encoding :utf-8)
                                    (seconds-until-expires #.(* 12 60 60))
                                    (signal-errors #t)
                                    &allow-other-keys)
  (with-thread-name " / SERVE-FILE"
    (remove-from-plistf args :signal-errors)
    (bind ((client-stream-dirty? #f))
      (handler-bind ((serious-condition (lambda (error)
                                          (unless signal-errors
                                            (server.warn "SERVE-FILE muffles the following error due to :signal-errors #f: ~A" error)
                                            (return-from serve-file (values #f error client-stream-dirty?))))))
        (with-open-file (file file-name :direction :input :element-type '(unsigned-byte 8))
          (unless for-download
            (unless content-type-p
              (setf content-type (or content-type
                                     (switch ((pathname-type file-name) :test #'string=)
                                       ;; this special-casing is an optimization to cons less
                                       ("html" (content-type-for +html-mime-type+ encoding))
                                       ("xml"  (content-type-for +xml-mime-type+ encoding))
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
          (setf client-stream-dirty? #t)
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
          (values #t nil #f))))))
