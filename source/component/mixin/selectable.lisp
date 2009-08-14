;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; selectable/mixin

(def (component e) selectable/mixin ()
  (#+nil ;; TODO: some instances are not selectable even though the class allows it
   (selectable-component
    #t
    :type boolean
    :initarg :hideable
    :computed-in compute-as
    :documentation "TRUE means COMPONENT can be SELECTED, FALSE otherwise."))
  (:documentation "A COMPONENT that can be SELECTED."))

;;;;;;
;;; selection/mixin

(def (component e) selection/mixin ()
  ((selected-component-set (compute-as (or -current-value- (make-hash-table))) :type (or null hash-table))
   (minimum-selection-cardinality 0 :type fixnum)
   (maximum-selection-cardinality 1 :type fixnum))
  (:documentation "A COMPONENT that maintains SELECTION."))

(def (generic e) single-selection-mode? (selection-component)
  (:method ((self selection/mixin))
    (= 1 (maximum-selection-cardinality-of self))))

(def (generic e) selected-component-of (selection-component)
  (:method ((self selection/mixin))
    (assert (single-selection-mode? self))
    (first (selected-components-of self))))

(def (generic e) selected-components-of (selection-component)
  (:method ((self selection/mixin))
    (awhen (selected-component-set-of self)
      (hash-table-values it))))

(def (generic e) (setf selected-components-of) (new-value selection-component)
  (:method (new-value (self selection/mixin))
    (bind ((selected-component-set (selected-component-set-of self)))
      (maphash-values #'mark-to-be-rendered-component selected-component-set)
      (clrhash selected-component-set)
      (dolist (component new-value)
        (mark-to-be-rendered-component component)
        (setf (gethash (hash-key-for component) selected-component-set) component))
      (invalidate-computed-slot self 'selected-component-set))))

(def (generic e) selected-component? (selection-component selectable-component)
  (:method ((selection-component selection/mixin) (selectable-component selectable/mixin))
    (awhen (selected-component-set-of selection-component)
      (gethash (hash-key-for selectable-component) it))))

(def (generic e) (setf selected-component?) (new-value selection-component selectable-component)
  (:method (new-value (selection-component selection/mixin) (selectable-component selectable/mixin))
    (bind ((selected-component-set (selected-component-set-of selection-component)))
      (mark-to-be-rendered-component selectable-component)
      (if new-value
          (setf (gethash (hash-key-for selectable-component) selected-component-set) selectable-component)
          (remhash (hash-key-for selectable-component) selected-component-set))
      (invalidate-computed-slot selection-component 'selected-component-set))))

(def (generic e) select-component (selectable-component class prototype value)
  (:method ((selectable-component selectable/mixin) class prototype value)
    (notf (selected-component? (find-selection-component selectable-component) selectable-component))))

(def (function e) find-selection-component (selectable-component)
  (find-ancestor-component-with-type selectable-component 'selection/mixin))

(def (function e) selectable-component-style-class (selectable-component)
  (when (selected-component? (find-selection-component selectable-component) selectable-component)
    " selected"))
