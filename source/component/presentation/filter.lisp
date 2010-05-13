;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/filter

(def (component e) component/filter (component/presentation)
  ()
  (:documentation "A filter searches for multiple existing values of TYPE based on a filter criteria provided by the user. This class does not have any slots on purpose.
  - similar to (select-instance ...)
  - static input
    - value-type: type
  - volatile input
    - all-values: sequence of type (implicit)
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
  - output
    - value: selected-type"))

;;;;;;
;;; t/filter

(def (component e) t/filter (component/filter t/presentation)
  ())

(def (generic e) collect-filter-predicates (component)
  (:method ((self t/filter))
    nil))

(def method component-dispatch-class ((self t/filter))
  (or (find-class-for-type (component-value-type-of self))
      (find-class t)))

;;;;;;
;;; Filter factory

(def (special-variable e) *filter-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *filter-type-mapping* nil nil)

(def layered-method make-filter (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *filter-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))
