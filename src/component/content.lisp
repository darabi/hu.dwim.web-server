;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content mixin

(def component content-mixin ()
  ((content nil :type component))
  (:documentation "A component that has another inside."))

(def render content-mixin
  (render (content-of -self-)))

(def refresh content-mixin
  (when-bind content (content-of -self-)
    (mark-outdated content)))

(def method component-value-of ((self content-mixin))
  (component-value-of (content-of self)))

(def method (setf component-value-of) (new-value (self content-mixin))
  (setf (component-value-of (content-of self)) new-value))

(def method find-command-bar ((component content-mixin))
  (or (call-next-method)
      (bind ((content (content-of component)))
        (ensure-uptodate content)
        (find-command-bar content))))

;;;;;;
;;; XHTML content mixin

(def component xhtml-content-mixin (content-mixin)
  ((content :type string))
  (:documentation "A component with an XHTML string content."))

(def render xhtml-content-mixin
  ;; TODO:
  (not-yet-implemented))

(def render-xhtml xhtml-content-mixin
  (write-sequence (babel:string-to-octets (content-of -self-) :encoding :utf-8) *xml-stream*)
  (values))

;;;;;;
;;; Content component

(def component content-component (title-context-menu-mixin commands-mixin content-mixin remote-identity-mixin user-messages-mixin)
  ()
  (:documentation "A component with a title, user messages, commands and another inside."))

(def function render-content (component)
  (bind (((:read-only-slots command-bar content id) component)
         (class-name (string-downcase (class-name (class-of component)))))
    (if (typep content '(or primitive-component reference-component))
        <span (:id ,id :class ,class-name)
              ,(render-user-messages component)
              ,(render content)>
        (progn
          <div (:id ,id :class ,class-name)
               ,(render-title component)
               ,(render-user-messages component)
               ,(render content)
               ,(render command-bar)>
          (render-remote-setup component)))))

(def render-xhtml content-component
  (render-content -self-))
