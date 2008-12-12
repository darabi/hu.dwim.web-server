;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Simple id/password login

(def (constant e :test 'string=) +login-identifier-cookie-name+ "login-identifier")
(def (constant e :test 'string=) +login-entry-point-path+ "login/")
(def (constant e :test 'string=) +session-timed-out-query-parameter-name+ "timed-out")
(def (constant e :test 'string=) +user-action-query-parameter-name+ "user-action")
(def (constant e :test 'string=) +continue-url-query-parameter-name+ "continue-url")

(def (component ea) identifier-and-password-login-component (title-component-mixin user-message-collector-component-mixin)
  ((identifier nil)
   (password nil)
   (command-bar (make-instance 'command-bar-component) :type component))
  (:default-initargs :title #"login.title"))

(def function make-default-identifier-and-password-login-command ()
  (command (icon login)
           (bind ((uri (make-application-relative-uri +login-entry-point-path+)))
             (setf (uri-query-parameter-value uri +user-action-query-parameter-name+) t)
             (copy-uri-query-parameters (uri-of *request*) uri +continue-url-query-parameter-name+)
             uri)
           :default #t))

(def (function e) make-identifier-and-password-login-component (&key (commands (list (make-default-identifier-and-password-login-command)))
                                                                     identifier password)
  (bind ((result (make-instance 'identifier-and-password-login-component :identifier identifier :password password)))
    (when commands
      (setf (commands-of (command-bar-of result)) (ensure-list commands)))
    result))

(def render identifier-and-password-login-component
  (bind (((:read-only-slots identifier password) -self-)
         (focused-field-id (if identifier
                               "password-field"
                               "identifier-field"))
         (id "login-component"))
    <div (:id ,id)
     ,(render-title -self-)
     ,(render-user-messages -self-)
     <table
       <tr <td (:class "label") ,#"login.identifier<>">
           <td (:class "value")
               <input (:id "identifier-field"
                           :name "identifier"
                           :value ,identifier)>>>
       <tr <td (:class "label") ,#"login.password<>">
           <td (:class "value")
               <input (:id "password-field"
                           :name "password"
                           :value ,password
                           :type "password")>>>
       <tr <td (:colspan 2)
             ,(render (command-bar-of -self-))>>>
     `js(on-load
         (.focus ($ ,focused-field-id))
         (wui.setup-login-component ,id))>))

(def (generic e) make-logout-command (application)
  (:method ((application application))
    (command (icon logout)
             (make-logout-action application)
             :send-client-state #f)))

(def (generic e) make-logout-action (application)
  (:method ((application application))
    (make-action
      (execute-logout-action application *session*)
      (make-redirect-response (make-uri-for-application *application* +login-entry-point-path+)))))

(def (generic e) execute-logout-action (application session)
  (:method (application session)
    ;; nop by default
    )
  (:method :after ((application application) (session session))
    (mark-session-invalid session)))

;;;;;;
;;; Fake login

(def (component e) fake-identifier-and-password-login-component ()
  ((identifier nil :export :accessor)
   (password nil :export :accessor)
   (comment nil))
  (:documentation "Useful to render one-click logins in test mode"))

(def render fake-identifier-and-password-login-component
  (bind (((:read-only-slots identifier password comment) -self-)
         (uri (clone-request-uri)))
    (setf (uri-query-parameter-value uri "identifier") identifier)
    (setf (uri-query-parameter-value uri "password") password)
    <div (:class "fake-login")
         <a (:href ,(print-uri-to-string uri))
            ,(concatenate-string identifier comment)>>))

(def (macro e) fake-identifier-and-password-login (identifier password &optional comment)
  `(make-instance 'fake-identifier-and-password-login-component :identifier ,identifier :password ,password :comment ,comment))

(def resources hu
  (login.title "Belépés")
  (login.identifier "Azonosító")
  (login.password "Jelszó")
  (login.message.authentication-failed "Azonosítás sikertelen")
  (login.message.session-timed-out "Lejárt a biztonsági idő, kérem lépjen be újra"))

(def resources en
  (login.title "Login")
  (login.identifier "Identifier")
  (login.password "Password")
  (login.message.authentication-failed "Authentication failed")
  (login.message.session-timed-out "Your session has timed out, please log in again"))
