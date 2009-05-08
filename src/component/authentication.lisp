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

(def (component ea) identifier-and-password-login-component (title-component-mixin
                                                             user-message-collector-component-mixin
                                                             remote-identity-component-mixin)
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
                                                                     identifier password title id)
  (bind ((result (make-instance 'identifier-and-password-login-component :identifier identifier :password password)))
    (when title
      (setf (title-of result) title))
    (when commands
      (setf (commands-of (command-bar-of result)) (ensure-list commands)))
    (when id
      (setf (id-of result) id))
    result))

(def render-xhtml identifier-and-password-login-component
  (bind (((:read-only-slots identifier password) -self-)
         (focused-field-id (if identifier
                               "password-field"
                               "identifier-field"))
         (id (id-of -self-)))
    <div (:id ,id
          :class "identifier-and-password-login-component")
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
         (wui.setup-widget "login-component" ,id))>))

(def (generic e) make-logout-command (application)
  (:method ((application application))
    (command (icon logout)
             (make-action
               (execute-logout application *session*)
               (make-redirect-response-for-current-application))
             :send-client-state #f)))

(def (generic e) execute-logout (application session)
  (:method (application session)
    ;; nop by default
    )
  (:method :after ((application application) (session session))
    (assert (eq *session* session))
    (mark-session-invalid session)
    ;; set *session* to nil so that the session cookie removal is decorated on the response. otherwise the next request to an entry point
    ;; would send up a session id to an invalid session and trigger HANDLE-REQUEST-TO-INVALID-SESSION.
    (setf *session* nil)))

;;;;;;
;;; Fake login

(def (component e) fake-identifier-and-password-login-component ()
  ((identifier nil :export :accessor)
   (password nil :export :accessor)
   (comment nil))
  (:documentation "Useful to render one-click logins in test mode"))

(def render-xhtml fake-identifier-and-password-login-component
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
