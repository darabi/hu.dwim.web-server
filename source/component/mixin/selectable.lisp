;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; selectable/mixin

(def (component e) selectable/mixin ()
  ((selectable-component
    #t
    :type boolean
    :initarg :selectable-component
    :computed-in computed-universe/session
    :documentation "TRUE means COMPONENT can be SELECTED, FALSE otherwise."))
  (:documentation "A COMPONENT that can be SELECTED."))

;;;;;;
;;; selection/mixin

(def (component e) selection/mixin ()
  ((selection
    (make-instance 'single-value-selection :selected-value (compute-as nil))
    #+nil
    (make-instance 'value-set-selection :selected-value-set (compute-as (or -current-value- (make-hash-table))))
    :type selection))
  (:documentation "A COMPONENT that maintains a SELECTION."))

(def (generic e) selected-component-of (selection-component)
  (:method ((self selection/mixin))
    (selected-single-value (selection-of self))))

(def (generic e) selected-components-of (selection-component)
  (:method ((self selection/mixin))
    (selected-value-set (selection-of self))))

(def (generic e) selected-component? (selection-component selectable-component)
  (:method ((selection-component selection/mixin) (selectable-component selectable/mixin))
    (selected-value? (selection-of selection-component) selectable-component)))

(def (generic e) (setf selected-component?) (new-value selection-component selectable-component)
  (:method (new-value (selection-component selection/mixin) (selectable-component selectable/mixin))
    (setf (selected-value? (selection-of selection-component) selectable-component) new-value)
    (mark-to-be-rendered-component selectable-component)
    (mark-to-be-rendered-component selection-component)))

(def (function e) find-selection-component (selectable-component)
  (find-ancestor-component-with-type selectable-component 'selection/mixin))

(def (function e) selected-component-value (selection-component)
  (awhen (selected-component-of selection-component)
    (component-value-of it)))

(def (function e) selectable-component-style-class (selectable-component)
  (when (selected-component? (find-selection-component selectable-component) selectable-component)
    " selected"))
