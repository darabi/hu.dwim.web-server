;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def function build-backtrace-string (error &key message)
  (hu.dwim.util:build-backtrace-string error :message message :timestamp (local-time:now)))

(def (generic e) handle-toplevel-error (context error)
  (:method :before (context (condition serious-condition))
    (maybe-invoke-debugger condition :context context))
  (:method :around (context error)
    (with-thread-activity-description ("HANDLE-TOPLEVEL-ERROR")
      (call-next-method))))

(def function is-error-from-client-stream? (error client-stream)
  (bind ((client-stream-fd (iolib:fd-of client-stream)))
    (or (and (typep error 'stream-error)
             (eq (stream-error-stream error) client-stream))
        ;; TODO the rest is fragile and easily breaks when iolib changes behavior
        (and client-stream-fd
             (or (and (typep error 'iolib:socket-error)
                      (eql client-stream-fd (isys:handle-of error)))
                 ;; TODO signalling non stream-error conditions might be an iolib bug
                 (and (typep error 'iolib.multiplex:poll-error)
                      (eql client-stream-fd (iolib.multiplex:poll-error-fd error))))))))

(def method debug-on-error? :around (context (condition access-denied-error))
  #f)

(def method handle-toplevel-error :before (broker (condition serious-condition))
  (maybe-invoke-debugger condition :context broker))

(def method handle-toplevel-error (broker (error serious-condition))
  (server.error (build-backtrace-string error))
  (cond
    ((null *request*)
     (server.info "Internal server error while the request it not yet parsed, so closing the socket as-is without sending any useful error message.")
     (abort-server-request "HANDLE-TOPLEVEL-ERROR bailed out without any response because *request* was not yet parsed"))
    ((or (not *response*)
         (not (headers-are-sent-p *response*)))
     (server.info "Sending an internal server error page for request ~S" (raw-uri-of *request*))
     (emit-simple-html-document-http-response (:status +http-internal-server-error+ :title #"error.internal-server-error.title")
       ;; TODO this *server* reference here is leaking from server/
       ;; that's why the usage of symbol-value...
       (bind ((args (list :administrator-email-address (and (boundp '*server*)
                                                            (administrator-email-address-of (symbol-value '*server*))))))
         (apply-localization-function 'render-internal-error-page args)))
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
            (apply-localization-function 'render-access-denied-error-page)))
        (server.info "Access denied for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri)))
  (abort-server-request "HANDLE-TOPLEVEL-ERROR succesfully handled the access denied error by sending an error page"))

