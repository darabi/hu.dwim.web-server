;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/viewer

(def (component e) component/viewer (component/presentation)
  ()
  (:documentation "A viewer displays existing values of a TYPE. This class does not have any slots on purpose.
  - similar to a #<LITERAL-OBJECT {100C204081}>
  - static input
    - value-type: type
  - volatile input
    - value: value-type
  - dispatch
    - dispatch-class: (class-of value)
    - dispatch-prototype: (class-prototype dispatch-class)
  - output
    - value: value-type"))

;;;;;;
;;; t/viewer

(def (component e) t/viewer (component/viewer t/presentation)
  ())

;;;;;;
;;; Viewer factory

(def layered-method make-viewer (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #f args))
