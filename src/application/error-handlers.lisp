;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; error in AJAX requests

(def method handle-toplevel-condition :around ((application application) error)
  (if (and (boundp '*ajax-aware-request*)
           *ajax-aware-request*)
      (emit-http-response ((+header/status+       +http-not-acceptable+
                            +header/content-type+ +xml-mime-type+))
        <ajax-response
         <error-message ,#"error-message-for-ajax-requests">
         <result "failure">>)
      (call-next-method)))


;;;;;;
;;; frame out of sync

(def method handle-toplevel-condition ((application application) (error frame-out-of-sync-error))
  (bind ((refresh-href   (print-uri-to-string (make-uri-for-current-frame)))
         (new-frame-href (print-uri-to-string (make-uri-for-new-frame)))
         (args (list refresh-href new-frame-href)))
    (emit-simple-html-document-response (:status +http-not-acceptable+
                                         :headers #.(list 'quote +disallow-response-caching-header-values+))
      (lookup-resource 'render-frame-out-of-sync-error
                       :arguments args
                       :otherwise (lambda ()
                                    (apply 'render-frame-out-of-sync-error/english args))))))

(def function render-frame-out-of-sync-error/english (refresh-href new-frame-href &key &allow-other-keys)
  <div
   <p "Browser window went out of sync with the server...">
   <p "Please avoid using the " <i>"Back"</i> " button of your browser and/or opening links in new windows by copy-pasting URL's or using the " <i>"Open in new window"</i> " feature of your browser. To achieve the same effect, you can use the navigation actions provided by the application.">
   <p <a (:href ,refresh-href) "Bring me back to the application">>
   <p <a (:href ,new-frame-href) "Reset my view of the application">>>)

(def resources en
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    (render-frame-out-of-sync-error/english refresh-href new-frame-href)))

(def resources hu
  (render-frame-out-of-sync-error (refresh-href new-frame-href &key &allow-other-keys)
    <div
     <p "A böngésző ablak nincs szinkronban a szerverrel...">
     <p "Ez egy komplex alkalmazás ami nem teljesen úgy működik mint egy átlagos weboldal, ezért kérjük ne használja a böngésző " <i>"Vissza"</i> " gombját és a " <i>"Megnyitás új ablakban"</i> " parancsot, valamint ne másoljon ki hivatkozásokat se! Kérjük, hogy az alkalmazás gombjaiat használja ezen műveletekhez!">
     <p <a (:href ,refresh-href) "Vissza az alkalmazáshoz">>
     <p <a (:href ,new-frame-href) "Új nézet az alkalmazásra">>>))


;;;;;;
;;; internal server error for applications

(defmethod handle-toplevel-condition ((application application) (error serious-condition))
  (if *session*
      (progn
        (log-error-with-backtrace error)
        (bind ((request-uri (raw-uri-of *request*)))
          (if (or (not *response*)
                  (not (headers-are-sent-p *response*)))
              (bind ((*response* nil) ; leave alone the original value, and keep an assert from firing
                     (rendering-in-progress *rendering-in-progress*))
                (server.info "Sending an internal server error page for request ~S coming to application ~A" request-uri application)
                (send-response
                 (make-component-rendering-response
                  (make-frame-component-with-content
                   application
                   (inline-component
                     (bind ((args (list (make-instance 'command-component
                                                       :icon (icon back)
                                                       :action (if rendering-in-progress
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
  ("error.internal-server-error" "Internal server error")
  (render-application-internal-error-page (&rest args &key &allow-other-keys)
    (apply 'render-application-internal-error-page/english args)))

(def resources hu
  ("error.internal-server-error" "Programhiba")
  (render-application-internal-error-page (back-command &key admin-email-address &allow-other-keys)
    <div
     <h1 "Programhiba">
     <p "A szerverhez érkezett kérés feldolgozása közben egy váratlan programhiba történt. Elnézést kérünk az esetleges kellemetlenségért!">
     <p "A hibáról értesülni fognak a fejlesztők és valószínűleg a közeljövőben javítják azt.">
     ,(when admin-email-address
        <p "Amennyiben kapcsolatba szeretne lépni az üzemeltetőkkel, azt a "
           <a (:href ,(mailto-href admin-email-address)) ,admin-email-address>
           " email címen megteheti.">)
     <p ,(render back-command)>>))
