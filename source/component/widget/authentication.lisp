;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; login/widget

(def (component e) login/widget ()
  ())

;;;;;;
;;; identifier-and-password-login/widget

;; TODO: this is kind separate from all the other components which is bad because it does not combine well with other features
;;       needs refactoring, less direct manipulation and more generalism through inheritance 
(def (component e) identifier-and-password-login/widget (login/widget
                                                         title/mixin
                                                         component-messages/widget
                                                         remote-setup/mixin)
  ((identifier nil)
   (password nil)
   (command-bar (make-instance 'command-bar/widget) :type component))
  (:default-initargs :title #"login.title"))

(def function make-default-identifier-and-password-login-command ()
  (command/widget (:default #t)
    (icon login)
    (bind ((uri (make-application-relative-uri +login-entry-point-path+)))
      (setf (uri-query-parameter-value uri +user-action-query-parameter-name+) t)
      (copy-uri-query-parameters (uri-of *request*) uri +continue-url-query-parameter-name+)
      uri)))

(def (function e) make-identifier-and-password-login/widget (&key (commands (list (make-default-identifier-and-password-login-command)))
                                                                  identifier password title id)
  (bind ((result (make-instance 'identifier-and-password-login/widget :identifier identifier :password password)))
    (when title
      (setf (title-of result) title))
    (when commands
      (setf (commands-of (command-bar-of result)) (ensure-list commands)))
    (when id
      (setf (id-of result) id))
    result))

(def render-xhtml identifier-and-password-login/widget
  (bind (((:read-only-slots id identifier password) -self-)
         (focused-field-id (if identifier
                               "password-field"
                               "identifier-field")))
    <div (:id ,id
          :class "identifier-and-password-login/widget")
     ,(render-title-for -self-)
     ,(render-component-messages-for -self-)
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
             ,(render-component (command-bar-of -self-))>>>
     `js(on-load
         (.focus ($ ,focused-field-id)))>))

(def (generic e) make-logout-command (application)
  (:method ((application application))
    (command/widget (:send-client-state #f)
      (icon logout)
      (make-action
        (execute-logout application *session*)
        (make-redirect-response-for-current-application)))))

;;;;;;
;;; fake-identifier-and-password-login/widget

(def (component e) fake-identifier-and-password-login/widget ()
  ((identifier nil)
   (password nil)
   (comment nil))
  (:documentation "Useful to render one-click logins in test mode"))

(def render-xhtml fake-identifier-and-password-login/widget
  (bind (((:read-only-slots identifier password comment) -self-)
         (uri (clone-request-uri)))
    (setf (uri-query-parameter-value uri "identifier") identifier)
    (setf (uri-query-parameter-value uri "password") password)
    <div (:class "fake-login")
         <a (:href ,(print-uri-to-string uri))
            ,(string+ identifier comment)>>))

(def (macro e) fake-identifier-and-password-login/widget (identifier password &optional comment)
  `(make-instance 'fake-identifier-and-password-login/widget :identifier ,identifier :password ,password :comment ,comment))

;;;;;;
;;; Icon

(def icon login)
(def icon logout)
