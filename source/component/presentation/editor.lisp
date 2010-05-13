;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/editor

(def (component e) component/editor (component/presentation)
  ()
  (:documentation "An editor edits existing values of a TYPE. This class does not have any slots on purpose.
  - similar to (reinitialize-instance ...)
  - static input
    - value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
  - dispatch
    - dispatch-class: (class-of value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - value: selected-type"))

;;;;;;
;;; t/editor

(def (component e) t/editor (component/editor t/presentation)
  ())

;;;;;;
;;; Editor factory

(def layered-method make-editor (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #t args))
