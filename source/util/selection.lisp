;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (generic e) selection? (selection)
  (:documentation "Returns TRUE if SELECTION is a selection, otherwise returns FALSE."))

(def (generic e) selection-minimum-cardinality (selection)
  (:documentation "Returns an integer greater than zero specifying the possible minimum number of selected values in SELECTION."))

(def (generic e) selection-maximum-cardinality (selection)
  (:documentation "Returns an integer greater than or equal to selection minimum cardinality. It specifies the possible maximum number of selected values in SELECTION."))

(def (generic e) selection-single-value? (selection)
  (:documentation "Returns TRUE if SELECTION can hold at most one selected value, otherwise returns FALSE."))

(def (generic e) selection-empty? (selection)
  (:documentation "Returns TRUE if SELECTION currently does not contain any selected value, otherwise returns FALSE."))

(def (generic e) selection-clear (selection)
  (:documentation "Removes all selected values from SELECTION making it empty."))

(def (generic e) selected-value? (selection value)
  (:documentation "Returns TRUE if SELECTION contains VALUE as a selected value, otherwise returns FALSE."))

(def (generic e) (setf selected-value?) (selected? selection value)
  (:documentation "Modifies the selected state of VALUE according to the SELECTED? boolean flag in SELECTION. In other words either adds or removes VALUE, or leaves the SELECTION unchanged."))

(def (generic e) selected-single-value (selection)
  (:documentation "Returns the currently selected value from SELECTION. The return value NIL indicates there's no such value. Signals an error if SELECTION is not a single selection."))

(def (generic e) (setf selected-single-value) (new-value selection)
  (:documentation "Changes the set of selected values to NEW-VALUE in SELECTION. Signals an error if the number of selected values is outside the allowed range."))

(def (generic e) selected-value-set (selection)
  (:documentation "Returns all selected values from SELECTION as a sequence. The return value NIL indicates there are no such values."))

(def (generic e) (setf selected-value-set) (new-value selection)
  (:documentation "Changes the set of selected values to the sequence NEW-VALUE in SELECTION. Signals an error if the number of selected values is outside the allowed range."))

(def (function e) make-selection (&key (minimum-cardinality 0) (maximum-cardinality most-positive-fixnum) (initial-values ()))
  "Creates a new selection with INITIAL-VALUES being the selected values in it."
  (prog1-bind selection
      (if (<= maximum-cardinality 1)
          (make-instance 'selection-single-value :mandatory (not (= minimum-cardinality 0)))
          (make-instance 'value-set-selection :minimum-cardinality minimum-cardinality :maximum-cardinality maximum-cardinality))
    (when initial-values
      (setf (selected-value-set selection) initial-values))))

;;;;;;
;;; selection

(def (class* e) selection ()
  ())

(def method selection? ((selection selection))
  #t)

;;;;;;
;;; selection-single-value

(def (computed-class* e) selection-single-value (selection)
  ((selected-value
    :computed-in t
    :type t)
   (mandatory
    #f
    :accessor mandatory?
    :type boolean)))

(def method selection-minimum-cardinality ((selection selection-single-value))
  (if (mandatory? selection)
      1
      0))

(def method selection-maximum-cardinality ((selection selection-single-value))
  1)

(def method selection-single-value? ((selection selection-single-value))
  #t)

(def method selection-empty? ((selection selection-single-value))
  (null (selected-value-of selection)))

(def method selection-clear ((selection selection-single-value))
  (setf (selected-value-of selection) nil))

(def method selected-value? ((selection selection-single-value) value)
  (eq (selected-value-of selection) value))

(def method (setf selected-value?) (selected? (selection selection-single-value) value)
  (if (eq (selected-value-of selection) value)
      (if selected?
          (values)
          (setf (selected-value-of selection) nil))
      (if selected?
          (setf (selected-value-of selection) value)
          (setf (selected-value-of selection) nil))))

(def method selected-single-value ((selection selection-single-value))
  (selected-value-of selection))

(def method (setf selected-single-value) (new-value (selection selection-single-value))
  (setf (selected-value-of selection) new-value))

(def method selected-value-set ((selection selection-single-value))
  (awhen (selected-value-of selection)
    (list it)))

(def method (setf selected-value-set) (new-value (selection selection-single-value))
  (cond ((length= 0 new-value)
         (setf (selected-value-of selection) nil))
        ((length= 1 new-value)
         (setf (selected-value-of selection) (first-elt new-value)))
        (t
         (error "Cannot set selected values to ~A in ~A" new-value selection))))

;;;;;;
;;; multiple-selection

(def (computed-class* e) value-set-selection (selection)
  ((selected-value-set
    :computed-in t
    :type (or null hash-table))
   (minimum-cardinality
    0
    :type fixnum)
   (maximum-cardinality
    most-positive-fixnum
    :type fixnum)))

(def method selection-minimum-cardinality ((selection value-set-selection))
  (minimum-cardinality-of selection))

(def method selection-maximum-cardinality ((selection value-set-selection))
  (maximum-cardinality-of selection))

(def method selection-single-value? ((selection value-set-selection))
  (<= (maximum-cardinality-of selection) 1))

(def method selection-empty? ((selection value-set-selection))
  (zerop (hash-table-count (selected-value-set-of selection))))

(def method selection-clear ((selection value-set-selection))
  (clrhash (selected-value-set-of selection))
  (invalidate-computed-slot selection 'selected-value-set))

(def method selected-value? ((selection value-set-selection) value)
  (gethash (hash-key value) (selected-value-set-of selection)))

(def method (setf selected-value?) (selected? (selection value-set-selection) value)
  (bind ((selected-value-set (selected-value-set-of selection))
         (key (hash-key value)))
    (if (gethash key selected-value-set)
        (if selected?
            (values)
            (remhash key selected-value-set))
        (if selected?
            (setf (gethash key selected-value-set) #t)
            (values)))))

(def method selected-single-value ((selection value-set-selection))
  (bind ((selected-value-set (selected-value-set-of selection)))
    (if (> (hash-table-count selected-value-set) 1)
        (error "~A contains multiple selected values" selection)
        (maphash-keys (lambda (selected-value)
                        (return-from selected-single-value selected-value))
                      selected-value-set))))

(def method (setf selected-single-value) (new-value (selection value-set-selection))
  (bind ((selected-value-set (selected-value-set-of selection)))
    (clrhash selected-value-set)
    (setf (gethash (hash-key new-value) selected-value-set) #t)
    (invalidate-computed-slot selection 'selected-value-set)))

(def method select-ed-value-set ((selection value-set-selection))
  (hash-table-keys (selected-value-set-of selection)))

(def method (setf selected-value-set) (new-value (selection value-set-selection))
  (bind ((selected-value-set (selected-value-set-of selection)))
    (clrhash selected-value-set)
    (iter (for selected-value :in-sequence new-value)
          (setf (gethash (hash-key selected-value) selected-value-set) #t))
    (invalidate-computed-slot selection 'selected-value-set)))
