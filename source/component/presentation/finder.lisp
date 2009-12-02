;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; finder/abstract

(def (component e) finder/abstract (presentation/abstract)
  ()
  (:documentation "A FINDER/ABSTRACT searches for a particular existing value of TYPE based on a filter criteria provided by the user.
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
;;; finder/minimal

(def (component e) finder/minimal (finder/abstract presentation/minimal)
  ())

;;;;;;
;;; finder/basic

(def (component e) finder/basic (finder/minimal presentation/basic)
  ())

;;;;;;
;;; finder/style

(def (component e) finder/style (finder/basic presentation/style)
  ())

;;;;;;
;;; finder/full

(def (component e) finder/full (finder/style presentation/full)
  ())

;;;;;;
;;; Finder factory

(def layered-method make-finder (type &rest args &key &allow-other-keys)
  (declare (ignore type args))
  (not-yet-implemented))
