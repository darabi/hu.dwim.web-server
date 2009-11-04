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
    :documentation "The current COMPONENT-VALUE that is presented by this COMPONENT."))
  (:documentation "A COMPONENT that presents a single COMPONENT-VALUE."))

(def method component-value-of ((self component-value/mixin))
  (slot-value self 'component-value))

(def method (setf component-value-of) (new-value (self component-value/mixin))
  (setf (slot-value self 'component-value) new-value))

(def layered-method refresh-component :before ((self component-value/mixin))
  (bind ((value (component-value-of self))
         (class (component-dispatch-class self))
         (prototype (component-dispatch-prototype self))
         (reused-value (reuse-component-value self class prototype value)))
    (unless (eq value reused-value)
      (setf (component-value-of self) reused-value))))
