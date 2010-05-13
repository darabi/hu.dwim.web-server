;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/finder

(def (component e) component/finder (component/presentation)
  ()
  (:documentation "A finder searches for a particular existing value of TYPE based on a filter criteria provided by the user. This class does not have any slots on purpose.
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
;;; t/finder

(def (component e) t/finder (component/finder t/presentation)
  ())

;;;;;;
;;; Finder factory

(def layered-method make-finder (type &rest args &key &allow-other-keys)
  (declare (ignore type args))
  (not-yet-implemented))
