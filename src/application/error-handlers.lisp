;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; error in AJAX requests

(def method handle-toplevel-condition :around (error (application application))
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

(defmethod handle-toplevel-condition ((error serious-condition) (application application))
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
                   (inline-component
                     (bind ((args (list (make-instance 'command-component
                                                       :content (icon back)
                                                       :action (if rendering-phase-reached
                                                                   (make-uri-for-new-frame)
                                                                   (make-uri-for-current-frame)))
                                        :admin-email-address (admin-email-address-of application))))
                       (lookup-resource 'render-application-internal-error-page
                                        :arguments args
                                        :otherwise (lambda ()
                                                     (apply 'render-application-internal-error-page/english args)))))))))
              (server.info "Internal server error for request ~S to application ~A and the headers are already sent, so closing the socket as-is without sending any useful error message." request-uri application)))
        (abort-server-request error))
      (call-next-method)))

(def function render-application-internal-error-page/english (back-command &key admin-email-address &allow-other-keys)
  <div
   <h1 "Internal server error">
   <p "An internal server error has occured, we are sorry for the inconvenience.">
   ,(when admin-email-address
          <p "You may contact the administrators at this email address: "
             <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>>)
   <p ,(render back-command)>>)

(def resources en
  (error.internal-server-error "Internal server error")
  (render-application-internal-error-page (&rest args &key &allow-other-keys)
    (apply 'render-application-internal-error-page/english args)))

(def resources hu
  (error.internal-server-error "Programhiba")
  (render-application-internal-error-page (back-command &key admin-email-address &allow-other-keys)
    <div
     <h1 "Programhiba">
     <p "Váratlan hiba történt az előző művelet végrehajtása közben. Elnézést kérünk az esetleges kellemetlenségért!">
     <p "A hibáról értesülni fognak a fejlesztők és várhatóan a közeljövőben javítják azt.">
     ,(when admin-email-address
        <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
           <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>
           " email címen megteheti.">)
     <p ,(render back-command)>>))
