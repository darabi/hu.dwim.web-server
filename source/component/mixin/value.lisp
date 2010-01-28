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

(def method clone-component ((self component-value/mixin))
  (aprog1 (call-next-method)
    (setf (component-value-of it) (component-value-of self))))

;;;;;;
;;; component-value-type/mixin

(def (component e) component-value-type/mixin ()
  ((component-value-type
    t
    :type t
    :documentation "The type of possible COMPONENT-VALUEs."))
  (:documentation "A COMPONENT that represents a single COMPONENT-VALUE of COMPONENT-VALUE-TYPE."))

(def method clone-component ((self component-value-type/mixin))
  (aprog1 (call-next-method)
    (setf (component-value-type-of it) (component-value-type-of self))))

;;;;;;
;;; selected-type/mixin

(def (component e) selected-type/mixin ()
  ((selected-type
    t
    :type t
    :documentation "The currently selected type for COMPONENT-VALUEs.")))

;;;;;;
;;; component-result/mixin

;; TODO: rename, move?
(def (component e) component-result/mixin ()
  ((result-limit 1 :type (or null number))
   (result (make-instance 'empty/layout) :type component)))

(def (function e) render-result-for (component)
  (awhen (result-of component)
    (render-component it)))

(def (layered-function e) make-result (component class prototype value)
  (:method ((component component-result/mixin) class prototype value)
    (make-inspector value)))
