;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; editor/abstract

(def (component e) editor/abstract (presentation/abstract)
  ()
  (:documentation "An EDITOR/ABSTRACT edits existing values of a TYPE.
  - similar to (reinitialize-instance ...)
  - static input
    - value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
  - dispatch
    - dispatch-class: (class-of component-value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - value: selected-type"))

;;;;;;
;;; editor/minimal

(def (component e) editor/minimal (editor/abstract presentation/minimal)
  ())

;;;;;;
;;; editor/basic

(def (component e) editor/basic (editor/minimal presentation/basic)
  ())

;;;;;;
;;; editor/style

(def (component e) editor/style (editor/basic presentation/style)
  ())

;;;;;;
;;; editor/full

(def (component e) editor/full (editor/style presentation/full)
  ())

;;;;;;
;;; Editor factory

(def layered-method make-editor (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #t args))
