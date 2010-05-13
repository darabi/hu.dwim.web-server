;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/inspector

(def (component e) component/inspector (component/presentation)
  ()
  (:documentation "An inspector displays or edits existing values of a TYPE. An inspector is essentially a viewer and an editor at the same time, and the user can switch between the two modes. This class does not have any slots on purpose.
  - similar to either #<LITERAL-OBJECT {100C204081}> or (reinitialize-instance ...)
  - static input
    - value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
  - dispatch
    - dispatch-class: (class-of value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - value: selected-type"))

;;;;;;
;;; t/inspector

(def (component e) t/inspector (component/inspector t/presentation editable/mixin)
  ())

(def method component-dispatch-class ((self t/inspector))
  (class-of (component-value-of self)))

;;;;;;
;;; Inspector factory

(def (special-variable e) *inspector-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *inspector-type-mapping* nil nil)

(def layered-method make-inspector (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *inspector-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))
