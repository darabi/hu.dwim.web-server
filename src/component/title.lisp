;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Title component

(def component title-component (content-mixin remote-setup-mixin)
  ()
  (:documentation "A component which represents the title of another component."))

(def (macro e) title (content)
  `(make-instance 'title-component :content ,content))

(def render title-component
  <span (:id ,(id-of -self-) :class "title")
    ,(render-title-icon (parent-component-of -self-))
    ,(call-next-method)>)

(def (layered-function e) render-title-icon (component)
  (:method ((component component))
    (values)))

;;;;;;
;;; Title mixin

(def component title-mixin ()
  ((title :type component))
  (:documentation "A component with a title component attached to it."))

(def refresh title-mixin
  (setf (title-of -self-) (make-title -self-)))

(def (layered-function e) make-title (component)
  (:documentation "Creates the title of a component.")

  (:method ((self title-mixin))
    ;; don't change by default
    (title-of self)))

(def layered-method render-title ((self title-mixin))
  (awhen (title-of self)
    (render it)))

;;;;;;
;;; Title context menu mixin

(def component title-context-menu-mixin (title-mixin context-menu-mixin)
  ())

(def layered-method render-title-icon ((self title-context-menu-mixin))
  (render (context-menu-of self)))
