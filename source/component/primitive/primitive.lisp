;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/presentation

(def (component e) primitive/presentation (presentation/abstract component/minimal renderable/mixin)
  ((name nil :type (or null symbol))
   (client-state-sink nil))
  (:documentation "Presentation for primitive types"))

(def refresh-component primitive/presentation
  (mark-to-be-rendered-component -self-))

(def render-csv primitive/presentation
  (write-csv-value (print-component-value -self-)))

(def render-ods primitive/presentation
  <text:p ,(print-component-value -self-) >)

(def generic print-component-value (component)
  (:documentation "Prints the COMPONENT-VALUE of COMPONENT into a STRING."))

(def generic parse-component-value (component client-value)
  (:documentation "Parses a STRING into the COMPONENT-VALUE of COMPONENT."))

(def generic string-field-type (component))

;;;;;;
;;; primitive/minimal

(def (component e) primitive/minimal (primitive/presentation component/minimal)
  ())

;;;;;;
;;; primitive/basic

(def (component e) primitive/basic (primitive/minimal component/basic)
  ())

;;;;;;
;;; primitive/style

(def (component e) primitive/style (primitive/basic component/style)
  ())

;;;;;;
;;; primitive/full

(def (component e) primitive/full (primitive/style component/full)
  ())
