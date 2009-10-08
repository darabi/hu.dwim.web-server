;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; empty/widget

(def (component e) empty/widget (widget/abstract parent/mixin)
  ()
  (:documentation "An EMPTY/WIDGET is completely empty, it is practically INVISIBLE. This component differs from EMPTY/LAYOUT in that it supports PARENT-COMPONENT-OF, so it can tell its place in the component hierarchy."))

(def (macro e) empty/widget ()
  '(make-instance 'empty/widget))

(def render-component empty/widget
  (values))

(def (function e) empty-widget? (component)
  (typep component 'empty/widget))
