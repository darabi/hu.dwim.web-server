;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) handle-toplevel-condition (error broker)
  (:method :around (error broker)
    (with-thread-name " / HANDLE-TOPLEVEL-CONDITION"
      (call-next-method))))

(def function is-error-from-client-stream? (error client-stream)
  (or (and (typep error 'stream-error)
           (eq (stream-error-stream error) client-stream))
      (and (typep error 'iolib:socket-error)
           (bind ((error-fd (isys:handle-of error)))
             (and error-fd
                  (eql error-fd (iolib:fd-of client-stream)))))))

(def function call-with-server-error-handler (thunk client-stream error-handler)
  (bind ((level-1-error nil))
    (labels ((level-1-error-handler (error)
               ;; first level of error handling, call around participants, give them a chance to render an error page, etc
               (setf level-1-error error)
               (handler-bind ((serious-condition #'level-2-error-handler))
                 (with-thread-name " / LEVEL-1-ERROR-HANDLER"
                   (cond
                     ((typep error 'storage-condition)
                      ;; on SBCL it includes control stack exhaustion, too
                      (server.error "Got a STORAGE-CONDITION, bailing out as quickly as possible with crossed fingers..."))
                     ((null *request*)
                      (server.warn "Ignoring error coming from the request parsing code in the belief that it's an illegal request. Remote host: ~A, error: ~A" *request-remote-host* error))
                     ((is-error-from-client-stream? error client-stream)
                      (server.debug "Ignoring stream error coming from the network stream: ~A" error))
                     (t
                       (server.debug "Calling custom error handler from CALL-WITH-SERVER-ERROR-HANDLER for error: ~A" error)
                       (funcall error-handler error)))
                   (abort-server-request "Level 1 error handler returned normally")
                   (error "Impossible code path in CALL-WITH-SERVER-ERROR-HANDLER / LEVEL-1-ERROR-HANDLER"))))
             (level-2-error-handler (error)
               ;; second level of error handling quarding against errors while handling the original error
               (handler-bind ((serious-condition #'level-3-error-handler))
                 (with-thread-name " / LEVEL-2-ERROR-HANDLER"
                   (if (is-error-from-client-stream? error client-stream)
                       (server.debug "Ignoring stream error coming from the network stream: ~A" error)
                       (progn
                         (log-error-with-backtrace error :message (list "Nested error while handling original error: ~A; the nested error is: ~A" level-1-error error))
                         (maybe-invoke-slime-debugger error)))
                   (abort-server-request error)
                   (error "Impossible code path in CALL-WITH-SERVER-ERROR-HANDLER / LEVEL-2-ERROR-HANDLER"))))
             (level-3-error-handler (error)
               ;; if we get here then do as little as feasible wrapped in ignore-errors to bail out and abort processing
               ;; the request as soon as we can.
               (with-thread-name " / LEVEL-3-ERROR-HANDLER"
                 (bind ((error-message (or (ignore-errors
                                             (format nil "Nested error while handling original error: ~A; the second nested error is: ~A"
                                                     level-1-error error))
                                           (ignore-errors
                                             (format nil "Failed to log nested error message (nested print errors?). Condition type of the third nested error is ~S."
                                                     (type-of error)))
                                           "Completely failed to log error, giving up... It's probably due to some nested printer errors or the the whole VM is dying...")))
                   (ignore-errors
                     (server.error error-message))
                   (abort-server-request error)
                   (error "Impossible code path in CALL-WITH-SERVER-ERROR-HANDLER / LEVEL-3-ERROR-HANDLER")))))
      (handler-bind
          ((serious-condition #'level-1-error-handler))
        (funcall thunk)))))

(def function maybe-invoke-slime-debugger (condition &key (context (or (and (boundp '*session*)
                                                                            *session*)
                                                                       (and (boundp '*application*)
                                                                            *application*)
                                                                       (and (boundp '*brokers*)
                                                                            (first *brokers*))))
                                                     (with-continue-restart #t))
  (when (debug-on-error context condition)
    (typecase condition
      ;; this is a place to skip errors that we don't want to catch in the debugger...
      (access-denied-error)
      (t (invoke-slime-debugger condition :with-continue-restart with-continue-restart)))))

(def function invoke-slime-debugger (condition &key (with-continue-restart #t))
  (if (or swank::*emacs-connection*
          (swank::default-connection))
      (flet ((body ()
               (server.debug "Invoking swank debugger for condition: ~A" condition)
               (swank:swank-debugger-hook condition nil)))
        (if with-continue-restart
            (restart-case
                (body)
              (continue ()
                :report "Continue processing the error (and probably send an error page)"))
            (body)))
      (server.debug "No Swank connection, not debugging error: ~A" condition)))

(def method handle-toplevel-condition :before ((error serious-condition) broker)
  (maybe-invoke-slime-debugger error :context broker))

(def method handle-toplevel-condition ((error serious-condition) broker)
  (log-error-with-backtrace error)
  (cond
    ((null *request*)
     (server.info "Internal server error while the request it not yet parsed, so closing the socket as-is without sending any useful error message.")
     (abort-server-request "HANDLE-TOPLEVEL-CONDITION bailed out without any response because *request* was not yet parsed"))
    ((or (not *response*)
         (not (headers-are-sent-p *response*)))
     (server.info "Sending an internal server error page for request ~S" (raw-uri-of *request*))
     (emit-simple-html-document-http-response (:status +http-internal-server-error+ :title #"error.internal-server-error.title")
       (bind ((args (list :admin-email-address (and (boundp '*server*)
                                                    (admin-email-address-of *server*)))))
         (apply-resource-function 'render-internal-error-page args)))
     (abort-server-request "HANDLE-TOPLEVEL-CONDITION succesfully handled the error by sending an error page"))
    (t
     (server.info "Internal server error for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." (raw-uri-of *request*))
     (abort-server-request "HANDLE-TOPLEVEL-CONDITION bailed out without any response because the HTTP headers are already sent")))
  (error "This HANDLE-TOPLEVEL-CONDITION method should never return"))

(def method handle-toplevel-condition ((error access-denied-error) broker)
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
  (abort-server-request "HANDLE-TOPLEVEL-CONDITION succesfully handled the access denied error by sending an error page"))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backtrace extraction

(def class* stack-frame ()
  ((description)
   (local-variables)
   (source-location)))

(def function make-stack-frame (description &optional source-location local-variables)
  (make-instance 'stack-frame
                 :description description
                 :source-location source-location
                 :local-variables local-variables))

(def special-variable *error-log-decorators* ())

(def (definer e :available-flags "eiod") error-log-decorator (name &body body)
  `(def function ,name ()
     ,@body))

(def (macro e) error-log-decorator (&body body)
  `(lambda ()
     ,@body))

(def (macro e) special-variable-printing-error-log-decorator (&rest variables)
  `(lambda ()
     (bind ((*package* (find-package "COMMON-LISP"))
            (*print-right-margin* 150))
       ,@(iter (for variable :in variables)
               (collect `(format t ,(bind ((*package* (find-package "COMMON-LISP")))
                                      (format nil "~~%~S: ~~A" variable))
                                 (if (boundp ',variable)
                                     ,variable
                                     'unbound)))))))

(def (macro e) with-error-log-decorator (decorator &body body)
  `(bind ((*error-log-decorators* (cons ,(if (symbolp decorator)
                                             `(quote ,decorator)
                                             decorator)
                                        *error-log-decorators*)))
     ,@body))

(def with-macro with-backtrace-bindings ()
  (bind ((rlist (reverse `((*print-pretty*           . #t)
                           (*print-level*            . 3)
                           (*print-length*           . 100)
                           (*print-circle*           . #t)
                           (*print-readably*         . #f)
                           (*print-pprint-dispatch*  . ,(copy-pprint-dispatch nil))
                           (*print-gensym*           . #t)
                           (*print-base*             . 10)
                           (*print-radix*            . #f)
                           (*print-array*            . #t)
                           (*print-lines*            . nil)
                           (*print-escape*           . #t)
                           (*print-right-margin*     . ,most-positive-fixnum))))
         (vars (mapcar #'car rlist))
         (vals (mapcar #'cdr rlist)))
    (progv vars vals
      (-body-))))

(def (function e) log-error-with-backtrace (error &key (logger (find-logger 'server)) (level +error+) message)
  (with-backtrace-bindings
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
        (bind ((log-line (with-output-to-string (*standard-output*)
                           (format t "~%*** At: ~A" (local-time:format-rfc3339-timestring nil (local-time:now)))
                           (when message
                             (format t "~%*** Message:~%")
                             (apply #'format t (ensure-list message)))
                           (format t "~%*** In thread: ~A~%*** Error:~%~A~%*** Backtrace:~%" (thread-name (current-thread)) error)
                           (bind ((backtrace (collect-backtrace))
                                  (*print-pretty* #f))
                             (iter (for stack-frame :in backtrace)
                                   (for index :upfrom 0)
                                   (write-string stack-frame)
                                   (terpri)))
                           (when *error-log-decorators*
                             (format t "~%*** Backtrace decorators:")
                             (dolist (decorator *error-log-decorators*)
                               (when (symbolp decorator)
                                 (bind ((*package* (find-package :keyword)))
                                   (format t "~%~S:" decorator)))
                               (funcall decorator)))
                           (format t "~%*** End of error details"))))
          (cl-yalog:handle-log-message logger log-line (elt cl-yalog::+log-level-names+ level)))))))

#*((:sbcl (def special-variable *special-variables-to-print-with-backtrace* '())
          (def special-variable *current-backtrace-special-variable-values*)

          (def function %print-special-variables-for-frame ()
            (with-output-to-string (stream)
              (bind ((found-one? #f))
                (dolist (var *special-variables-to-print-with-backtrace*)
                  (bind (((:values previous-value found?) (gethash var *current-backtrace-special-variable-values*))
                         (current-value (if (boundp var)
                                            (symbol-value var)
                                            'unbound)))
                    (when (or (not found?)
                              (not (eq previous-value current-value)))
                      (setf (gethash var *current-backtrace-special-variable-values*) current-value)
                      (unless found-one?
                        (setf found-one? #t)
                        (format stream "~%---- Special variables follow:"))
                      (bind ((printed-value (or (ignore-errors
                                                  (princ-to-string current-value))
                                                "<error printing value>")))
                        (format stream "~%---- ~S: ~A" var printed-value))))))))

          (def function collect-backtrace (&key (start 4) (count sb-debug::*default-backtrace-size-limit*)
                                                ((:verbosity sb-debug::*verbosity*) sb-debug::*verbosity*)
                                                (print-frame-source (> sb-debug::*verbosity* 1))
                                                &allow-other-keys)
            (bind ((backtrace ())
                   (*current-backtrace-special-variable-values* (make-hash-table :test 'eq)))
              (sb-debug::map-backtrace
               (lambda (frame)
                 (bind ((frame-as-string (with-output-to-string (stream)
                                           (handler-case
                                               (progn
                                                 (sb-debug::print-frame-call frame stream :number #t
                                                                             :print-frame-source print-frame-source)
                                                 #+nil ; TODO eval-in-frame does not eval with the proper dynamic environment, so this is pretty useless for now
                                                 (write-string (funcall (the function
                                                                          (sb-di:preprocess-for-eval '(%print-special-variables-for-frame) (sb-di:frame-code-location frame)))
                                                                        frame)
                                                               stream))
                                             (serious-condition (error)
                                               ;; NOTE: the usage of ~S is important here to avoid calling
                                               ;; any custom PRINT-OBJECT methods that may error again.
                                               (format nil "<<< Error while printing frame: ~S >>>" error))))))
                   (push frame-as-string backtrace)))
               :start start :count count)
              (nreversef backtrace)
              backtrace)))

   (t (def function collect-backtrace (&key (start 4) (count 500) &allow-other-keys)
        (bind ((swank::*buffer-package* *package*))
          (swank-backend:call-with-debugging-environment
           (lambda ()
             (iter (for (index description) :in (swank:backtrace start (+ start count)))
                   (collect (format nil "~3,'0D: ~A" index description)))))))))
