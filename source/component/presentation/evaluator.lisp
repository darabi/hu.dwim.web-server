;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; evaluator/abstract

(def (component e) evaluator/abstract (presentation/abstract)
  ()
  (:documentation "An EVALUATOR/ABSTRACT TODO:
  - similar to an arbitrary form
  - static input
    - form: form
    - free-variable-types: list of types
    - return-type: type
  - volatile input
    - free-variable-values: list of
  - output
    - value: return-type"))

;;;;;;
;;; evaluator/minimal

(def (component e) evaluator/minimal (evaluator/abstract presentation/minimal)
  ())

;;;;;;
;;; evaluator/basic

(def (component e) evaluator/basic (evaluator/minimal presentation/basic)
  ())

;;;;;;
;;; evaluator/style

(def (component e) evaluator/style (evaluator/basic presentation/style)
  ())

;;;;;;
;;; evaluator/full

(def (component e) evaluator/full (evaluator/style presentation/full)
  ())
