;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; invoker/component

(def (component e) invoker/component (component/presentation)
  ()
  (:documentation "An invoker ... This class does not have any slots on purpose.
  - similar to a function call such as (foo ...)
  - static input
    - function-names: list of symbols
    - argument-types: list of types
    - return-type: type
  - volatile input
    - argument-values: list of
  - output
    - value: return-type"))

;;;;;;
;;; t/invoker

(def (component e) t/invoker (invoker/component t/presentation)
  ())
