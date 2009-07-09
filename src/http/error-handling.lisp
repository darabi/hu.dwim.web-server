;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) handle-toplevel-error (context error)
  (:method :before (context (condition serious-condition))
    (maybe-invoke-debugger condition :context context))
  (:method :around (context error)
    (with-thread-activity-description ("HANDLE-TOPLEVEL-ERROR")
      (call-next-method))))

(def function is-error-from-client-stream? (error client-stream)
  (or (and (typep error 'stream-error)
           (eq (stream-error-stream error) client-stream))
      (and (typep error 'iolib:socket-error)
           (bind ((error-fd (isys:handle-of error)))
             (and error-fd
                  (eql error-fd (iolib:fd-of client-stream)))))))

(def method debug-on-error? :around (context (condition access-denied-error))
  #f)

(def method handle-toplevel-error :before (broker (condition serious-condition))
  (maybe-invoke-debugger condition :context broker))

(def method handle-toplevel-error (broker (error serious-condition))
  (log-error-with-backtrace error)
  (cond
    ((null *request*)
     (server.info "Internal server error while the request it not yet parsed, so closing the socket as-is without sending any useful error message.")
     (abort-server-request "HANDLE-TOPLEVEL-ERROR bailed out without any response because *request* was not yet parsed"))
    ((or (not *response*)
         (not (headers-are-sent-p *response*)))
     (server.info "Sending an internal server error page for request ~S" (raw-uri-of *request*))
     (emit-simple-html-document-http-response (:status +http-internal-server-error+ :title #"error.internal-server-error.title")
       (bind ((args (list :admin-email-address (and (boundp '*server*)
                                                    (admin-email-address-of *server*)))))
         (apply-resource-function 'render-internal-error-page args)))
     (abort-server-request "HANDLE-TOPLEVEL-ERROR succesfully handled the error by sending an error page"))
    (t
     (server.info "Internal server error for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." (raw-uri-of *request*))
     (abort-server-request "HANDLE-TOPLEVEL-ERROR bailed out without any response because the HTTP headers are already sent")))
  (error "This HANDLE-TOPLEVEL-ERROR method should never return"))

(def method handle-toplevel-error (broker (error access-denied-error))
  (bind ((request-uri (if *request*
                          (raw-uri-of *request*)
                          "<unavailable>")))
    (if (or (not *response*)
            (not (headers-are-sent-p *response*)))
        (progn
          (server.info "Sending an access denied error page for request ~S" request-uri)
          (emit-simple-html-document-http-response (:status +http-forbidden+ :title #"error.access-denied-error")
            (apply-resource-function 'render-access-denied-error-page)))
        (server.info "Access denied for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri)))
  (abort-server-request "HANDLE-TOPLEVEL-ERROR succesfully handled the access denied error by sending an error page"))

(def (function e) log-error-with-backtrace (error &key (logger (find-logger 'server)) (level +error+) message)
  (handler-bind ((serious-condition
                  (lambda (nested-error)
                    (handler-bind ((serious-condition
                                    (lambda (nested-error2)
                                      (declare (ignore nested-error2))
                                      (ignore-errors
                                        (cl-yalog:handle-log-message logger
                                                                     (list "Failed to log backtrace due to another nested error...")
                                                                     '+error+)))))
                      (cl-yalog:handle-log-message logger
                                                   (list (format nil "Failed to log backtrace due to: ~A. The orignal error is: ~A" nested-error error))
                                                   '+error+)))))
    (setf logger (find-logger logger))
    (when (cl-yalog::enabled-p logger level)
      (bind ((log-line (build-backtrace-string error
                                               :message (when message
                                                          (apply #'format t (ensure-list message)))
                                               :timestamp (local-time:now))))
        (cl-yalog:handle-log-message logger log-line (elt cl-yalog::+log-level-names+ level))))))
