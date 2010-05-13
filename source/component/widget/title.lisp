;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; title/widget

(def (component e) title/widget (standard/widget content/component)
  ()
  (:documentation "A TITLE/WIDGET represents the TITLE of another COMPONENT."))

(def (macro e) title/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'title/widget ,@args :content ,(the-only-element content)))

(def method component-style-class ((self title/widget))
  (string+ "title-border " (call-next-method)))

(def render-component title/widget
  (render-content-for -self-))

(def render-xhtml title/widget
  (with-render-style/component (-self-)
    (render-content-for -self-)))

;;;;;;
;;; title-bar/widget

(def (component e) title-bar/widget (standard/widget title/mixin)
  ()
  (:documentation "A COMPONENT that has a TITLE and various other small widgets around it."))

(def (macro e) title-bar/widget ((&rest args &key &allow-other-keys) &body title)
  `(make-instance 'title-bar/widget ,@args :title ,(the-only-element title)))

(def method component-style-class ((self title-bar/widget))
  (string+ "title-border " (call-next-method)))

(def render-xhtml title-bar/widget
  (bind ((parent-component (parent-component-of -self-)))
    (with-render-style/component (-self- :element-name "span")
      (render-collapse-or-expand-command-for parent-component)
      (render-show-context-menu-command-for parent-component)
      (render-title-for -self-)
      (render-hide-command-for parent-component))))
