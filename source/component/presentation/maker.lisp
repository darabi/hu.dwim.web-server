;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; maker/abstract

(def (component e) maker/abstract (presentation/abstract)
  ()
  (:documentation "A MAKER/ABSTRACT creates new values of a TYPE.
  - similar to (make-instance ...)
  - static input
    - value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
    - restrictions: (e.g. initargs)
  - output
    - value: selected-type"))

;;;;;;
;;; maker/minimal

(def (component e) maker/minimal (maker/abstract presentation/minimal)
  ())

;;;;;;
;;; maker/basic

(def (component e) maker/basic (maker/minimal presentation/basic)
  ())

;;;;;;
;;; maker/style

(def (component e) maker/style (maker/basic presentation/style)
  ())

;;;;;;
;;; maker/full

(def (component e) maker/full (maker/style presentation/full)
  ())

;;;;;;
;;; Maker factory

(def special-variable *maker-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *maker-type-mapping* nil nil)

(def (layered-function e) find-maker-type (type)
  (:method (type)
    (linear-mapping-value *maker-type-mapping* type)))

(def layered-method make-maker (type &rest args &key value &allow-other-keys)
  (bind ((component-type (find-maker-type type)))
    (apply #'make-instance component-type
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs (find-class component-type) args))))
