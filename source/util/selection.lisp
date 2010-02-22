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

(def (generic e) selection-cardinality-check (selection new-count)
  (:documentation "Returns TRUE if SELECTION is allowed to have the number of selected values specified by NEW-COUNT, otherwise returns FALSE in which case the selection is not changed. The default implementation signals a continuable error if NEW-COUNT is not between the values returned by SELECTION-MINIMUM-CARDINALITY and SELECTION-MAXIMUM-CARDINALITY."))

(def (generic e) selection-empty? (selection)
  (:documentation "Returns TRUE if SELECTION currently does not contain any selected value, otherwise returns FALSE."))

(def (generic e) selection-count (selection)
  (:documentation "Returns the number of selected values in SELECTION."))

(def (generic e) selection-clear (selection)
  (:documentation "Removes all selected values from SELECTION making it empty."))

(def (generic e) selection-select (selection value)
  (:documentation "Selects VALUE in SELECTION, does nothing if VALUE is already selected."))

(def (generic e) selection-deselect (selection value)
  (:documentation "Deselects VALUE in SELECTION, does nothing if VALUE is not selected."))

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
          (make-instance 'single-value-selection :mandatory (not (= minimum-cardinality 0)))
          (make-instance 'value-set-selection :minimum-cardinality minimum-cardinality :maximum-cardinality maximum-cardinality))
    (when initial-values
      (setf (selected-value-set selection) initial-values))))

;;;;;;
;;; selection

(def (class* e) selection ()
  ())

(def (condition* e) selection-error ()
  ((selection :type selection))
  (:report
   (lambda (error stream)
     (format stream "Error during processing operation on ~A" (selection-of error)))))

(def method selection? ((selection selection))
  #t)

(def method selection-cardinality-check ((selection selection) (new-count integer))
  (if (<= (selection-minimum-cardinality selection) new-count (selection-maximum-cardinality selection))
      #t
      (progn
        (cerror "Ignore selection change" 'selection-error :selection selection)
        #f)))

(def method selection-empty? ((selection selection))
  (null (selected-value-set selection)))

(def method selection-count ((selection selection))
  (length (selected-value-set selection)))

(def method selection-clear :around ((selection selection))
  (when (selection-cardinality-check selection 0)
    (call-next-method)))

(def method selection-clear ((selection selection))
  (setf (selected-value-set selection) ())
  (values))

(def method selection-select ((selection selection) value)
  (setf (selected-value? selection value) #t)
  (values))

(def method selection-deselect ((selection selection) value)
  (setf (selected-value? selection value) #f)
  (values))

(def method selected-value? ((selection selection) value)
  (member value (selected-value-set selection)))

(def method (setf selected-value?) (selected? (selection selection) value)
  (bind ((selected-values (selected-value-set selection)))
    (if (member value selected-values)
        (if selected?
            (values)
            (setf (selected-value-set selection) (remove value selected-values)))
        (if selected?
            (setf (selected-value-set selection) (cons value selected-values))
            (values))))
  selected?)

(def method selected-single-value ((selection selection))
  (bind ((selected-values (selected-value-set selection)))
    (if (or (null selected-values)
            (length= selected-values 1))
        (first selected-values)
        (error "~A contains multiple selected values" selection))))

(def method (setf selected-single-value) (new-value (selection selection))
  (setf (selected-value-set selection) (list new-value))
  new-value)

(def method (setf selected-single-value) :around (new-value (selection selection))
  (when (selection-cardinality-check selection 1)
    (call-next-method)))

(def method (setf selected-value-set) :around (new-value (selection selection))
  (when (selection-cardinality-check selection (length new-value))
    (call-next-method)))

;;;;;;
;;; single-value-selection

(def (computed-class* e) single-value-selection (selection)
  ((selected-value
    :computed-in t
    :type t)
   (mandatory
    #f
    :accessor mandatory?
    :type boolean)))

(def method selection-minimum-cardinality ((selection single-value-selection))
  (if (mandatory? selection)
      1
      0))

(def method selection-maximum-cardinality ((selection single-value-selection))
  1)

(def method selection-empty? ((selection single-value-selection))
  (null (selected-value-of selection)))

(def method selection-count ((selection single-value-selection))
  (if (selected-value-of selection)
      0
      1))

(def method selection-clear ((selection single-value-selection))
  (setf (selected-value-of selection) nil)
  (values))

(def method selected-value? ((selection single-value-selection) value)
  (eq (selected-value-of selection) value))

(def method (setf selected-value?) (selected? (selection single-value-selection) value)
  (if (eq (selected-value-of selection) value)
      (if selected?
          (values)
          (when (selection-cardinality-check selection 0)
            (setf (selected-value-of selection) nil)))
      (if selected?
          (when (selection-cardinality-check selection 1)
            (setf (selected-value-of selection) value))
          (when (selection-cardinality-check selection 0)
            (setf (selected-value-of selection) nil))))
  selected?)

(def method selected-single-value ((selection single-value-selection))
  (selected-value-of selection))

(def method (setf selected-single-value) (new-value (selection single-value-selection))
  (setf (selected-value-of selection) new-value))

(def method selected-value-set ((selection single-value-selection))
  (awhen (selected-value-of selection)
    (list it)))

(def method (setf selected-value-set) (new-value (selection single-value-selection))
  (cond ((length= 0 new-value)
         (setf (selected-value-of selection) nil))
        ((length= 1 new-value)
         (setf (selected-value-of selection) (first new-value)))
        (t
         (error "Cannot set selected values to ~A in ~A" new-value selection)))
  new-value)

;;;;;;
;;; value-set-selection

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

(def method selection-empty? ((selection value-set-selection))
  (zerop (hash-table-count (selected-value-set-of selection))))

(def method selection-count ((selection value-set-selection))
  (hash-table-count (selected-value-set-of selection)))

(def method selection-clear ((selection value-set-selection))
  (clrhash (selected-value-set-of selection))
  (invalidate-computed-slot selection 'selected-value-set)
  (values))

(def method selected-value? ((selection value-set-selection) value)
  (gethash (hash-key value) (selected-value-set-of selection)))

(def method (setf selected-value?) (selected? (selection value-set-selection) value)
  (bind ((selected-value-set (selected-value-set-of selection))
         (count (hash-table-count selected-value-set))
         (key (hash-key value)))
    (if (gethash key selected-value-set)
        (if selected?
            (values)
            (when (selection-cardinality-check selection (1- count))
              (remhash key selected-value-set)))
        (if selected?
            (when (selection-cardinality-check selection (1+ count))
              (setf (gethash key selected-value-set) #t))
            (values)))
    (invalidate-computed-slot selection 'selected-value-set)
    selected?))

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
    (invalidate-computed-slot selection 'selected-value-set)
    new-value))

(def method selected-value-set ((selection value-set-selection))
  (hash-table-keys (selected-value-set-of selection)))

(def method (setf selected-value-set) (new-value (selection value-set-selection))
  (bind ((selected-value-set (selected-value-set-of selection)))
    (clrhash selected-value-set)
    (iter (for selected-value :in-sequence new-value)
          (setf (gethash (hash-key selected-value) selected-value-set) #t))
    (invalidate-computed-slot selection 'selected-value-set)
    new-value))
