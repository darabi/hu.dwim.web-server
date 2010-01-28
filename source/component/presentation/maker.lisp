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

(def method component-dispatch-class ((self maker/abstract))
  (or (find-class-for-type (component-value-type-of self))
      (find-class t)))

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

(def (special-variable e) *maker-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *maker-type-mapping* nil nil)

(def layered-method make-maker (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *maker-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))
