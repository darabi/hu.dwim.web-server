;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Simple id/password login

(def (constant e :test 'string=) +login-entry-point-path+ "login/")

(def (component ea) identifier-and-password-login-component (user-message-collector-component-mixin)
  ((identifier nil)
   (password nil)
   (command-bar (make-instance 'command-bar-component) :type component)))

(def function make-default-identifier-and-password-login-command ()
  (command (icon login)
           (bind ((uri (make-application-relative-uri +login-entry-point-path+)))
             (setf (uri-query-parameter-value uri "user-action") t)
             (setf (uri-query-parameter-value uri "continue-uri")
                   (uri-query-parameter-value (uri-of *request*) "continue-uri"))
             uri)))

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
                               "identifier-field")))
    <div (:id "login-component")
     ,(render-user-messages -self-)
     <table
       <tr <td ,#"login.identifier<>">
           <td <input (:id "identifier-field"
                       :name "identifier"
                       :value ,identifier)>>>
       <tr <td ,#"login.password<>">
           <td <input (:id "password-field"
                       :name "password"
                       :value ,password
                       :type "password")>>>
       <tr <td (:colspan 2)
             ,(render (command-bar-of -self-))>>>
     `js(on-load
         (.focus ($ ,focused-field-id)))>))

;;;;;;
;;; Fake login

(def (component ea) fake-identifier-and-password-login-component ()
  ((identifier nil)
   (password nil)
   (comment nil))
  (:documentation "Useful to render one-click logins in test mode"))

(def render fake-identifier-and-password-login-component
  (bind (((:read-only-slots identifier password comment) -self-)
         (uri (clone-uri (uri-of *request*))))
    (setf (uri-query-parameter-value uri "identifier") identifier)
    (setf (uri-query-parameter-value uri "password") password)
    <div (:class "fake-login")
         <a (:href ,(print-uri-to-string uri))
            ,(concatenate-string identifier comment)>>))

(def (macro e) fake-identifier-and-password-login (identifier password &optional comment)
  `(make-instance 'fake-identifier-and-password-login-component :identifier ,identifier :password ,password :comment ,comment))

(defresources hu
  (login.identifier "Azonosító")
  (login.password "Jelszó"))

(defresources en
  (login.identifier "Identifier")
  (login.password "Password"))
