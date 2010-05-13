;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/selector

(def (component e) component/selector (component/presentation)
  ()
  (:documentation "A selector displays all existing values of TYPE at once to select exactly one VALUE of them. This class does not have any slots on purpose.
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
;;; t/selector

(def (component e) t/selector (component/selector t/presentation)
  ())

;;;;;;
;;; Selector factory

(def layered-method make-selector (type &rest args &key &allow-other-keys)
  (declare (ignore type args))
  (not-yet-implemented))
