;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def special-variable *current-condition*)

(def (generic e) handle-toplevel-condition (broker error))

(defun call-with-server-error-handler (thunk network-stream error-handler)
  (handler-bind
      ((serious-condition
        (lambda (error)
          (with-thread-name " / CALL-WITH-SERVER-ERROR-HANDLER"
            (bind ((parent-condition (when (boundp '*current-condition*)
                                       *current-condition*))
                   (*current-condition* error))
              (if parent-condition
                  (let ((error-message (or (ignore-errors
                                             (format nil "Nested error while handling error: ~A, the second error is ~A"
                                                     parent-condition error))
                                           (ignore-errors
                                             (format nil "Failed to log nested error message, probably due to nested print errors. Condition type is ~S."
                                                     (type-of error)))
                                           "Failed to log nested error message, probably due to some nested print errors.")))
                    (ignore-errors
                      (server.error error-message))
                    ;; let the error fall through and most probably reach the toplevel debugger if left enabled.
                    ;; there's really nothing else we could do here, because silently aborting the request
                    ;; and pretending that nothing bad happened could lead to server hangs, especially with
                    ;; stack overflow errors. entering the toplevel debugger or exiting with an error code is
                    ;; still better than a silent hang...
                    )
                  (progn
                    (if (and (typep error 'stream-error)
                             (eq (stream-error-stream error) network-stream))
                        (server.debug "Ignoring stream error coming from the network stream: ~A" error)
                        (progn
                          (server.debug "Calling custom error handler from CALL-WITH-SERVER-ERROR-HANDLER for error: ~A" error)
                          (funcall error-handler error)))
                    (abort-server-request error)
                    (assert nil nil "Impossible code path in CALL-WITH-SERVER-ERROR-HANDLER"))))))))
    (funcall thunk)))

(defun invoke-slime-debugger-if-possible (condition)
  (if (or swank::*emacs-connection*
          (swank::default-connection))
      (progn
        (server.debug "Invoking swank debugger for condition: ~A" condition)
        (swank:swank-debugger-hook condition nil))
      (server.debug "No Swank connection, not debugging error: ~A" condition)))

(def function log-error-with-backtrace (error)
  (server.error "~%*** At: ~A~%*** In thread: ~A~%*** Error:~%~A~%*** Backtrace is:~%~A"
                (local-time:format-rfc3339-timestring (local-time:now))
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

(defmethod handle-toplevel-condition :before (broker (error serious-condition))
  (when (debug-on-error broker error)
    (restart-case
         (invoke-slime-debugger-if-possible error)
      (continue ()
        :report "Continue processing the error (and probably send an error page)"
        (return-from handle-toplevel-condition)))))

(defmethod handle-toplevel-condition (broker (error serious-condition))
  (log-error-with-backtrace error)
  (when (boundp '*request*)
    (send-standard-error-page :condition error))
  (abort-server-request error))

(defun send-standard-error-page (&key condition (message "An internal server error has occured." message-p)
                                 (title message) (http-status-code +http-internal-server-error+))
  (server.info "Sending ~A for request ~S" http-status-code (if (boundp '*request*)
                                                                (path-of (uri-of *request*))
                                                                "?"))
  (when (and (not message-p)
             condition)
    (ignore-errors
      (setf message (with-output-to-string (str)
                      (princ condition str)))))
  (if (or (not (boundp '*response*))
          (not (headers-are-sent-p *response*)))
      (render-standard-error-page :message message :title title :http-status-code http-status-code)
      (server.error "The headers are already sent, closing the socket as-is without sending any useful error message")))

(defun render-standard-error-page (&key (message "An internal server error has occured.")
                                        (title message) (http-status-code +http-internal-server-error+))
  (emit-http-response ((+header/status+       http-status-code
                        +header/content-type+ +html-content-type+))
    (with-html-document-body (:title title :content-type +html-content-type+)
      <h1 ,title>
      <p ,message>)))


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
