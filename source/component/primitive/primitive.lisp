;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/presentation

(def (component e) primitive/presentation (t/presentation standard/component renderable/mixin)
  ((name nil :type (or null symbol))
   (client-state-sink nil))
  (:documentation "The base class for presentation of primitive types."))

(def refresh-component primitive/presentation
  (mark-to-be-rendered-component -self-))

(def render-component primitive/presentation
  (render-component (print-component-value -self-)))

(def generic print-component-value (component)
  (:documentation "Prints the COMPONENT-VALUE of COMPONENT into a STRING."))

(def function render-component-value (component)
  <span (:id ,(id-of component)) ,(print-component-value component)>)

(def generic parse-component-value (component client-value)
  (:documentation "Parses a STRING into the COMPONENT-VALUE of COMPONENT."))

(def generic string-field-type (component))
