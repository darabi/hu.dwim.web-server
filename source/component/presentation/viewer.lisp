;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; viewer/abstract

(def (component e) viewer/abstract (presentation/abstract)
  ()
  (:documentation "A VIEWER/ABSTRACT displays existing values of a TYPE.
  - similar to a #<LITERAL-OBJECT {100C204081}>
  - static input
    - component-value-type: type
  - volatile input
    - component-value: component-value-type
  - dispatch
    - dispatch-class: (class-of component-value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - component-value: component-value-type"))

;;;;;;
;;; viewer/minimal

(def (component e) viewer/minimal (viewer/abstract presentation/minimal)
  ())

;;;;;;
;;; viewer/basic

(def (component e) viewer/basic (viewer/minimal presentation/basic)
  ())

;;;;;;
;;; viewer/style

(def (component e) viewer/style (viewer/basic presentation/style)
  ())

;;;;;;
;;; Viewer factory

(def layered-method make-viewer (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #f args))
