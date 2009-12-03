;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; inspector/abstract

(def (component e) inspector/abstract (presentation/abstract editable/mixin)
  ()
  (:documentation "An INSPECTOR/ABSTRACT displays or edits existing values of a TYPE. An inspector is essentially a viewer and an editor at the same time, and the user can switch between the two modes.
  - similar to either #<LITERAL-OBJECT {100C204081}> or (reinitialize-instance ...)
  - static input
    - component-value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of component-value-type)
    - component-value: selected-type
  - dispatch
    - dispatch-class: (class-of component-value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - component-value: selected-type"))

;;;;;;
;;; inspector/minimal

(def (component e) inspector/minimal (inspector/abstract presentation/minimal)
  ())

;;;;;;
;;; inspector/basic

(def (component e) inspector/basic (inspector/minimal presentation/basic)
  ())

;;;;;;
;;; inspector/style

(def (component e) inspector/style (inspector/basic presentation/style)
  ())

;;;;;;
;;; inspector/full

(def (component e) inspector/full (inspector/style presentation/full)
  ())

;;;;;;
;;; Inspector factory

(def special-variable *inspector-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *inspector-type-mapping* nil nil)

(def layered-method make-inspector (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *inspector-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))
