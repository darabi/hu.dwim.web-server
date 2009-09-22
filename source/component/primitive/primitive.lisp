;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/abstract

(def (component e) primitive/abstract (component/minimal component-value/mixin)
  ((name nil :type (or null symbol))
   (the-type nil)
   (client-state-sink nil)))

(def render-csv primitive/abstract
  (write-csv-value (print-component-value -self-)))

(def render-ods primitive/abstract
  <text:p ,(print-component-value -self-) >)

(def method component-value-of :around ((self primitive/abstract))
  (bind ((result (call-next-method)))
    (if (typep result 'error)
        (error result)
        result)))

(def generic parse-component-value (component client-value)
  (:documentation "Parses a STRING into the COMPONENT-VALUE of COMPONENT."))

(def generic print-component-value (component)
  (:documentation "Prints the COMPONENT-VALUE of COMPONENT into a STRING."))

(def generic string-field-type (component))

;;;;;;
;;; primitive/minimal

(def (component e) primitive/minimal (primitive/abstract component/minimal)
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
