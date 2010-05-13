;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/evaluator

(def (component e) component/evaluator (component/presentation)
  ()
  (:documentation "An evaluator ... This class does not have any slots on purpose.
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
;;; t/evaluator

(def (component e) t/evaluator (component/evaluator t/presentation)
  ())
