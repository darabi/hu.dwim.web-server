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

(def (layered-function e) find-filter-type (type)
  (:method (type)
    (linear-mapping-value *filter-type-mapping* type)))

(def layered-method make-filter (type &rest args &key value &allow-other-keys)
  (bind ((component-type (find-filter-type type)))
    (apply #'make-instance component-type
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs (find-class component-type) args))))

;;;;;;
;;; Filter interface

(def (generic e) collect-filter-predicates (component))
