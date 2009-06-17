;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Handle conditions in AJAX requests

(def method handle-toplevel-condition :around (condition (application application))
  (if (and (boundp '*ajax-aware-request*)
           *ajax-aware-request*)
      (progn
        (maybe-invoke-slime-debugger error)
        (emit-http-response ((+header/status+       +http-not-acceptable+
                              +header/content-type+ +xml-mime-type+))
          <ajax-response
           <error-message ,#"error-message-for-ajax-requests">
           <result "failure">>))
      (call-next-method)))

;;;;;;
;;; internal server error for applications

(def component internal-error-message-component (content-component)
  ((rendering-phase-reached :type boolean))
  (:default-initargs :title #"error.internal-server-error.title" :id "internal-error"))

(def layered-method make-command-bar-commands ((self internal-error-message-component) class prototype value)
  (list* (make-instance 'command/basic
                        :content (icon back)
                        :action (if (rendering-phase-reached-p self)
                                    (make-uri-for-new-frame)
                                    (make-uri-for-current-frame)))
         (call-next-method)))

(def method handle-toplevel-condition ((error serious-condition) (application application))
  (when (and (not *inside-user-code*)
             *session*)
    ;; oops, this error comes from inside WUI or at least not from an action or from render. let's try to invalidate the session if there's any...
    (server.warn "Invalidating session ~A because an error came from inside WUI while processing a request coming to it" *session*)
    (mark-session-invalid *session*)
    (setf *session* nil))
  (if (and *session*
           *inside-user-code*)
      (progn
        (log-error-with-backtrace error)
        (bind ((request-uri (raw-uri-of *request*)))
          (if (or (not *response*)
                  (not (headers-are-sent-p *response*)))
              (bind ((*response* nil) ; leave alone the original value, and keep an assert from firing
                     (rendering-phase-reached *rendering-phase-reached*))
                (server.info "Sending an internal server error page for request ~S coming to application ~A" request-uri application)
                (send-response
                 (make-component-rendering-response
                  (make-frame-component-with-content
                   application
                   (aprog1
                       (make-instance 'internal-error-message-component
                                      :rendering-phase-reached rendering-phase-reached
                                      :content (inline-render
                                                 (apply-resource-function 'render-application-internal-error-page
                                                                          (list :admin-email-address (admin-email-address-of application)))))
                     (add-user-error it #"error.internal-server-error.message"))))))
              (server.info "Internal server error for request ~S to application ~A and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri application)))
        (abort-server-request error))
      (call-next-method)))
