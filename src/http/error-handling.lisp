;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def (generic e) handle-toplevel-condition (error broker)
  (:method :around (error broker)
    (with-thread-name " / HANDLE-TOPLEVEL-CONDITION"
      (call-next-method))))

(def function is-error-from-client-stream? (error &optional (client-stream (client-stream-of *request*)))
  (or (and (typep error 'stream-error)
           (eq (stream-error-stream error) client-stream))
      (and (typep error 'iolib:socket-error)
           (bind ((error-fd (isys:handle-of error)))
             (and error-fd
                  (eql error-fd (fd-of client-stream)))))))

(defun call-with-server-error-handler (thunk client-stream error-handler)
  (bind ((level-1-error nil))
    (labels ((level-1-error-handler (error)
               ;; first level of error handling, call around participants, give them a chance to render an error page, etc
               (setf level-1-error error)
               (handler-bind ((serious-condition #'level-2-error-handler))
                 (with-thread-name " / LEVEL-1-ERROR-HANDLER"
                   (if (is-error-from-client-stream? error client-stream)
                       (server.debug "Ignoring stream error coming from the network stream: ~A" error)
                       (progn
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
                         (server.error "Nested error while handling original error: ~A; the nested error is: ~A. Backtrace follows..." level-1-error error)
                         (log-error-with-backtrace error)
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

(def function log-error-with-backtrace (error)
  (server.error "~%*** At: ~A~%*** In thread: ~A~%*** Error:~%~A~%*** Backtrace:~%~A"
                (local-time:format-rfc3339-timestring nil (local-time:now))
                (thread-name (current-thread))
                error
                (let ((backtrace (collect-backtrace error))
                      (*print-pretty* #f))
                  (with-output-to-string (str)
                    (iter (for stack-frame :in backtrace)
                          (for index :upfrom 0)
                          (format str "~3,'0D: " index)
                          (write-string (description-of stack-frame) str)
                          (terpri str))))))

(defmethod handle-toplevel-condition :before ((error serious-condition) broker)
  (maybe-invoke-slime-debugger error :context broker))

(defmethod handle-toplevel-condition ((error serious-condition) broker)
  (log-error-with-backtrace error)
  (bind ((request-uri (if *request*
                          (raw-uri-of *request*)
                          "<unavailable>")))
    (if (or (not *response*)
            (not (headers-are-sent-p *response*)))
        (progn
          (server.info "Sending an internal server error page for request ~S" request-uri)
          (emit-simple-html-document-http-response (:status +http-internal-server-error+ :title #"error.internal-server-error")
            (bind ((args (list :admin-email-address (and (boundp '*server*)
                                                         (admin-email-address-of *server*)))))
              (lookup-resource 'render-internal-error-page
                               :arguments args
                               :otherwise (lambda ()
                                            (apply 'render-internal-error-page/english args))))))
        (server.info "Internal server error for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri)))
  (abort-server-request "HANDLE-TOPLEVEL-CONDITION succesfully handled the error by sending an error page"))

(def function render-internal-error-page/english (&key admin-email-address &allow-other-keys)
  <div
   <h1 "Internal server error">
   <p "An internal server error has occured while processing your request. We are sorry for the inconvenience.">
   <p "The developers will be notified about this error and will hopefully fix it in the near future.">
   ,(when admin-email-address
      <p "You may contact the administrators at the "
         <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>
         " email address.">)
   <p <a (:href `js-inline(history.go -1)) "Go back">>>)

(defmethod handle-toplevel-condition ((error access-denied-error) broker)
  (bind ((request-uri (if *request*
                          (raw-uri-of *request*)
                          "<unavailable>")))
    (if (or (not *response*)
            (not (headers-are-sent-p *response*)))
        (progn
          (server.info "Sending an access denied error page for request ~S" request-uri)
          (emit-simple-html-document-http-response (:status +http-forbidden+ :title #"error.access-denied-error")
            (lookup-resource 'render-access-denied-error-page
                             :otherwise (lambda ()
                                          (render-access-denied-error-page/english)))))
        (server.info "Access denied for request ~S and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri)))
  (abort-server-request "HANDLE-TOPLEVEL-CONDITION succesfully handled the access denied error by sending an error page"))

(def function render-access-denied-error-page/english (&key &allow-other-keys)
  <div
   <h1 "Access denied">
   <p "You have no permission to access the requested resource.">
   <p <a (:href `js-inline(history.go -1)) "Go back">>>)


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backtrace extraction

(def class* stack-frame ()
  ((description)
   (local-variables)
   (source-location)))

(defun make-stack-frame (description &optional source-location local-variables)
  (make-instance 'stack-frame
                 :description description
                 :source-location source-location
                 :local-variables local-variables))

(defun collect-backtrace (condition &key (start 4) (end 500))
  (let ((swank::*swank-debugger-condition* condition)
        (swank::*buffer-package* *package*))
    (swank-backend:call-with-debugging-environment
     (lambda ()
       (iter (for (index description) :in (swank:backtrace start end))
             (collect (make-stack-frame description
                                        (ignore-errors
                                          (if (numberp index)
                                              (swank-backend:frame-source-location-for-emacs index)
                                              index))
                                        (swank-backend:frame-locals index))))))))
