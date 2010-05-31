;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; login/widget

(def (component e) login/widget (component-messages/widget
                                 title/mixin
                                 command-bar/mixin)
  ())

(def layered-method make-command-bar-commands ((component login/widget) class prototype value)
  (optional-list* (make-login-command component class prototype value)
                  ;; TODO: this would add a refresh-component command, uncomment when refresh-component is removed from widgets
                  #+nil (call-next-layered-method) nil))

(def (layered-function e) make-login-command (component class prototype value))

(def method component-style-class ((self login/widget))
  (string+ "content-border " (call-next-method)))

;;;;;;
;;; identifier-and-password-login/widget

;; TODO: this is kind separate from all the other components which is bad because it does not combine well with other features
;;       needs refactoring, less direct manipulation and more generalism through inheritance
;; TODO rename to something that tells that this component works without sessions
(def (component e) identifier-and-password-login/widget (login/widget)
  ((identifier nil)
   (password nil))
  (:default-initargs :title (title/widget () #"login.title")))

(def (macro e) identifier-and-password-login/widget (&rest args &key &allow-other-keys)
  `(make-instance 'identifier-and-password-login/widget ,@args))

(def render-xhtml identifier-and-password-login/widget
  (bind (((:read-only-slots identifier password) -self-)
         (focused-field-id (if identifier
                               "password-field"
                               "identifier-field")))
    (with-render-style/component (-self-)
      (render-title-for -self-)
      (render-component-messages-for -self-)
      <table
        <tr <td (:class "label") ,#"slot-name.identifier<>">
          <td (:class "value")
            <input (:id "identifier-field"
                    :name "identifier"
                    :value ,identifier)>>>
        <tr <td (:class "label") ,#"slot-name.password<>">
          <td (:class "value")
            <input (:id "password-field"
                    :name "password"
                    :value ,password
                    :type "password")>>>
        <tr <td (:colspan 2) ,(render-command-bar-for -self-)>>>)
    `js-onload(.focus ($ ,focused-field-id))))

(def layered-method make-login-command ((component identifier-and-password-login/widget) class prototype value)
  (command/widget (:default #t)
    (icon/widget login)
    (bind ((uri (make-uri-for-current-application +login-entry-point-path+)))
      (setf (uri-query-parameter-value uri +user-action-query-parameter-name+) t)
      (copy-uri-query-parameters (uri-of *request*) uri +continue-url-query-parameter-name+)
      uri)))

(def (generic e) make-logout-command (application)
  (:method :before (application)
    (assert (eq *application* application)))

  (:method ((application application))
    (command/widget (:ajax #f :send-client-state #f)
      (icon/widget logout)
      (make-action
        (logout *application* *session*)
        (decorate-session-cookie *application* (make-redirect-response-for-current-application))))))

;;;;;;
;;; login-data/login/inspector

(def (component e) login-data/login/inspector (t/name-value-list/inspector login/widget)
  ())

(def method component-style-class ((self login-data/login/inspector))
  (string+ "content-border " (call-next-method)))

(def render-xhtml login-data/login/inspector
  (with-render-style/component (-self-)
    (render-component-messages-for -self-)
    (render-content-for -self-)
    (render-command-bar-for -self-)))

(def layered-method make-login-command ((component login-data/login/inspector) (class standard-class) (prototype login-data) (value login-data))
  (when (authorize-operation *application* '(make-login-command))
    (unless (is-logged-in? *session*)
      (command/widget (:default #t :ajax #f)
        (icon/widget login)
        (make-action
          (store-editing component)
          (handler-case
              (progn
                (login *application* *session* (component-value-of component))
                (clear-root-component))
            (error/authentication ()
              (add-component-error-message component #"login.message.authentication-failed"))))))))

;;;;;;
;;; fake-identifier-and-password-login/widget

(def (component e) fake-identifier-and-password-login/widget (standard/widget)
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
(def (icon e) cancel-impersonalization)
