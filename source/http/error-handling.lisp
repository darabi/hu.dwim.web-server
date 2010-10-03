;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def function network-stream-has-been-written-to? ()
  (and *response*
       (headers-are-sent-p *response*)))

(def function is-error-worth-logging? (error)
  (not (typep error '(or access-denied-error
                         illegal-http-request/error))))

(def function build-backtrace-string (error &key message)
  (hu.dwim.util:build-backtrace-string error :message message :timestamp (local-time:now)))

(def function is-error-from-client-stream? (error client-stream)
  (bind ((client-stream-fd (iolib:fd-of client-stream)))
    (or (and (typep error 'stream-error)
             (eq (stream-error-stream error) client-stream))
        ;; TODO the rest is fragile and easily breaks when iolib changes behavior
        ;; TODO follow iolib when it gets the condition cleanup
        ;; TODO signalling non stream-error conditions from stream operations might be an iolib bug
        (and client-stream-fd
             (or (and (typep error 'iolib.syscalls:syscall-error)
                      (eql client-stream-fd (isys:handle-of error)))
                 (and (typep error 'iolib.multiplex:poll-error)
                      (eql client-stream-fd (iolib.multiplex:poll-error-fd error))))))))

(def method debug-on-error? :around (context (condition access-denied-error))
  #f)

(def methods handle-toplevel-error

  (:method :before (context (error serious-condition))
    (bind ((message (build-backtrace-string error :message "HANDLE-TOPLEVEL-ERROR :before is now dealing with this error")))
      (if (is-error-worth-logging? error)
          (server.error message)
          (server.dribble message)))
    (maybe-invoke-debugger error :context context))

  (:method :around (context error)
    (with-thread-activity-description ("HANDLE-TOPLEVEL-ERROR")
      (call-next-method)))

  (:method (context (error serious-condition))
    (cond
      ((null *request*)
       (server.info "Internal server error while the request it not yet parsed, so closing the socket as-is without sending any useful error message.")
       (abort-server-request "HANDLE-TOPLEVEL-ERROR bailed out without any response because *request* was not yet parsed"))
      ((not (network-stream-has-been-written-to?))
       (handle-toplevel-error/emit-response context error)
       (abort-server-request "HANDLE-TOPLEVEL-ERROR succesfully handled the error by calling HANDLE-TOPLEVEL-ERROR/EMIT-RESPONSE"))
      (t
       (server.info "Internal server error for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." (raw-uri-of *request*))
       (abort-server-request "HANDLE-TOPLEVEL-ERROR bailed out without any response because the HTTP headers are already sent")))
    (error "This HANDLE-TOPLEVEL-ERROR method should never return"))

  (:method (context (error access-denied-error))
    (bind ((request-uri (if *request*
                            (raw-uri-of *request*)
                            "<unavailable>")))
      (if (network-stream-has-been-written-to?)
          (server.info "Access denied for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri)
          (handle-toplevel-error/emit-response context error)))
    (abort-server-request "HANDLE-TOPLEVEL-ERROR succesfully handled the access denied error by sending an error page")))

(def methods handle-toplevel-error/emit-response

  (:method :around (context error)
    (with-thread-activity-description ("HANDLE-TOPLEVEL-ERROR/EMIT-RESPONSE")
      (call-next-method)))

  (:method (context (error serious-condition))
    (server.info "Sending an internal server error page for request ~S" (raw-uri-of *request*))
    (emit-http-response/simple-html-document (:status +http-internal-server-error+
                                              :title #"error.internal-server-error.title"
                                              :cacheable #f)
      ;; TODO this *server* reference here is leaking from server/
      ;; that's why the usage of symbol-value...
      (bind ((args (list :administrator-email-address (and (boundp '*server*)
                                                           (administrator-email-address-of (symbol-value '*server*))))))
        (apply-localization-function 'render-error-page/internal-error args))))

  (:method (context (error access-denied-error))
    (bind ((request-uri (if *request*
                            (raw-uri-of *request*)
                            "<unavailable>")))
      (server.info "Sending an access denied error page for request ~S" request-uri)
      (emit-http-response/simple-html-document (:status +http-forbidden+
                                                :title #"error.access-denied-error"
                                                :cacheable #f)
        (apply-localization-function 'render-error-page/access-denied)))))
