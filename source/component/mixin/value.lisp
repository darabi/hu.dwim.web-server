;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component-value/mixin

(def (component e) component-value/mixin ()
  ((component-value
    :type t
    :computed-in compute-as
    :documentation "The current COMPONENT-VALUE."))
  (:documentation "A COMPONENT that represents a single COMPONENT-VALUE."))

(def layered-method refresh-component :before ((self component-value/mixin))
  (bind ((value (component-value-of self))
         (class (component-dispatch-class self))
         (prototype (component-dispatch-prototype self))
         (reused-value (reuse-component-value self class prototype value)))
    (unless (eq value reused-value)
      (setf (component-value-of self) reused-value))))

;;;;;;
;;; component-value-type/mixin

(def (component e) component-value-type/mixin ()
  ((component-value-type
    t
    :type t
    :documentation "The type of possible COMPONENT-VALUEs."))
  (:documentation "A COMPONENT that represents a single COMPONENT-VALUE of COMPONENT-VALUE-TYPE."))
