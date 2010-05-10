;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;; TODO: the selection is lost on refresh-component

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

(def (generic e) select-component (selection-component selectable-component)
  (:method ((selection-component selection/mixin) (selectable-component selectable/mixin))
    (setf (selected-component? selection-component selectable-component) #t)))

(def (generic e) deselect-component (selection-component selectable-component)
  (:method ((selection-component selection/mixin) (selectable-component selectable/mixin))
    (setf (selected-component? selection-component selectable-component) #f)))

(def (generic e) selected-component? (selection-component selectable-component)
  (:method ((selection-component selection/mixin) (selectable-component selectable/mixin))
    (selected-value? (selection-of selection-component) selectable-component)))

(def (generic e) (setf selected-component?) (new-value selection-component selectable-component)
  (:method (new-value (selection-component selection/mixin) (selectable-component selectable/mixin))
    (bind ((selection (selection-of selection-component))
           (old-selected-components (selected-value-set selection)))
      (setf (selected-value? selection selectable-component) new-value)
      (bind ((new-selected-components (selected-value-set selection)))
        (foreach #'mark-to-be-rendered-component (set-difference (union old-selected-components new-selected-components)
                                                                 (intersection old-selected-components new-selected-components)))))))

(def (function eo) find-selection-component (selectable-component &key (otherwise :error otherwise?))
  (or (find-ancestor-component-of-type 'selection/mixin selectable-component :otherwise #f)
      (handle-otherwise (error "~S failed; starting from ~A" 'find-selection-component selectable-component))))

(def (function e) selected-component-value (selection-component)
  (awhen (selected-component-of selection-component)
    (component-value-of it)))

(def (function e) selectable-component-style-class (selectable-component)
  (string+ (when (selected-component? (find-selection-component selectable-component) selectable-component)
             " selected")
           (when (selectable-component? selectable-component)
             " selectable")))
