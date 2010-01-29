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
;; TODO rename to something that tells that this component works without sessions
;; TODO make a potentially smarter counterpart that uses features which require a session/frame?
(def (component e) identifier-and-password-login/widget (login/widget
                                                         title/mixin
                                                         command-bar/mixin
                                                         component-messages/widget
                                                         remote-setup/mixin)
  ((identifier nil)
   (password nil))
  (:default-initargs :title #"login.title"))

(def (macro e) identifier-and-password-login/widget (&rest args &key &allow-other-keys)
  `(make-instance 'identifier-and-password-login/widget ,@args))

(def layered-method make-command-bar-commands ((component identifier-and-password-login/widget) class prototype value)
  (list (make-default-identifier-and-password-login-command)))

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
       <tr <td (:colspan 2) ,(render-command-bar-for -self-)>>>
     `js(on-load
         (.focus ($ ,focused-field-id)))>))

(def function make-default-identifier-and-password-login-command ()
  (command/widget (:default #t)
    (icon login)
    (bind ((uri (make-uri-for-current-application +login-entry-point-path+)))
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

(def (generic e) make-logout-command (application)
  (:method ((application application))
    (assert (eq *application* application))
    (command/widget (:send-client-state #f)
      (icon logout)
      (make-action
        (logout application *session*)
        (make-redirect-response-for-current-application)))))

;;;;;;
;;; fake-identifier-and-password-login/widget

(def (component e) fake-identifier-and-password-login/widget (widget/style)
  ((identifier nil)
   (password nil)
   (comment nil))
  (:documentation "Useful to render one-click logins in test mode"))

(def (macro e) fake-identifier-and-password-login/widget (identifier password &optional comment)
  `(make-instance 'fake-identifier-and-password-login/widget :identifier ,identifier :password ,password :comment ,comment))

(def render-xhtml fake-identifier-and-password-login/widget
  (bind (((:read-only-slots identifier password comment) -self-)
         (uri (clone-request-uri)))
    (setf (uri-query-parameter-value uri "identifier") identifier)
    (setf (uri-query-parameter-value uri "password") password)
    <div (:class "fake-login")
         <a (:href ,(print-uri-to-string uri))
            ,(string+ identifier comment)>>))

;;;;;;
;;; Icon

(def (icon e) login)

(def (icon e) logout)
