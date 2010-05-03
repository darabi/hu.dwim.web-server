;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; invoker/abstract

(def (component e) invoker/abstract (presentation/abstract)
  ()
  (:documentation "An INVOKER/ABSTRACT TODO:
  - similar to (foo ...)
  - static input
    - function-names: list of symbols
    - argument-types: list of types
    - return-type: type
  - volatile input
    - argument-values: list of
  - output
    - value: return-type"))

;;;;;;
;;; invoker/minimal

(def (component e) invoker/minimal (invoker/abstract presentation/minimal)
  ())

;;;;;;
;;; invoker/basic

(def (component e) invoker/basic (invoker/minimal presentation/basic)
  ())

;;;;;;
;;; invoker/style

(def (component e) invoker/style (invoker/basic presentation/style)
  ())
