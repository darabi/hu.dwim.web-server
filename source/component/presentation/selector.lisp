;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; selector/abstract

(def (component e) selector/abstract (presentation/abstract)
  ()
  (:documentation "A SELECTOR/ABSTRACT displays all existing values of TYPE at once to select exactly one VALUE of them.
  - similar to (elt ...)
  - static input
    - value-type: type
  - volatile input
    - all-values: sequence of type (implicit)
    - selected-type: type (selected-type is a subtype of value-type)
    - value: value-type
  - output
    - value: value-type"))

;;;;;;
;;; selector/minimal

(def (component e) selector/minimal (selector/abstract presentation/minimal)
  ())

;;;;;;
;;; selector/basic

(def (component e) selector/basic (selector/minimal presentation/basic)
  ())

;;;;;;
;;; selector/style

(def (component e) selector/style (selector/basic presentation/style)
  ())

;;;;;;
;;; Selector factory

(def layered-method make-selector (type &rest args &key &allow-other-keys)
  (declare (ignore type args))
  (not-yet-implemented))
