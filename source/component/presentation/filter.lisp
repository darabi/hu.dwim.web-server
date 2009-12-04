;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; filter/abstract

(def (component e) filter/abstract (presentation/abstract)
  ()
  (:documentation "A FILTER/ABSTRACT searches for multiple existing values of TYPE based on a filter criteria provided by the user.
  - similar to (select-instance ...)
  - static input
    - value-type: type
  - volatile input
    - all-values: sequence of type (implicit)
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
  - output
    - value: selected-type"))

(def method collect-filter-predicates ((self filter/abstract))
  nil)

(def method component-dispatch-class ((self filter/abstract))
  (or (find-class-for-type (component-value-type-of self))
      (find-class t)))

;;;;;;
;;; filter/minimal

(def (component e) filter/minimal (filter/abstract presentation/minimal)
  ())

;;;;;;
;;; filter/basic

(def (component e) filter/basic (filter/minimal presentation/basic)
  ())

;;;;;;
;;; filter/style

(def (component e) filter/style (filter/basic presentation/style)
  ())

;;;;;;
;;; filter/full

(def (component e) filter/full (filter/style presentation/full)
  ())

;;;;;;
;;; Filter factory

(def special-variable *filter-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *filter-type-mapping* nil nil)

(def layered-method make-filter (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *filter-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))

;;;;;;
;;; Filter interface

(def (generic e) collect-filter-predicates (component))
