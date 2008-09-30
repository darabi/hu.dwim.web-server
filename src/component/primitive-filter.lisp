;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ())

(def render :before primitive-filter
  (ensure-client-state-sink -self-))

(def layered-function render-filter-predicate (component)
  (:method ((self component))
    <td>
    <td>))

;;;;;;
;;; T filter

(def component t-filter (t-component primitive-filter)
  ())

;;;;;;
;;; Boolean filter

(def component boolean-filter (boolean-component primitive-filter)
  ())

(def render boolean-filter ()
  <select ()
    <option "">
    ,(unless (eq (the-type-of -self-) 'boolean)
             <option ,#"value.nil">)
    <option ,#"boolean.true">
    <option ,#"boolean.false">>)

;;;;;;
;;; String filter

(def component string-filter (string-component primitive-filter)
  ((component-value nil)))

(def render string-filter ()
  (render-string-field -self-))

;;;;;;
;;; Password filter

(def component password-filter (password-component string-filter)
  ())

;;;;;;
;;; Symbol filter

(def component symbol-filter (symbol-component string-filter)
  ())

;;;;;;
;;; Number filter

(def component number-filter (number-component primitive-filter)
  ())

;;;;;;
;;; Integer filter

(def component integer-filter (integer-component number-filter)
  ())

;;;;;;
;;; Float filter

(def component float-filter (float-component number-filter)
  ())

;;;;;;
;;; Date filter

(def component date-filter (date-component primitive-filter)
  ())

;;;;;;
;;; Time filter

(def component time-filter (time-component primitive-filter)
  ())

;;;;;;
;;; Timestamp filter

(def component timestamp-filter (timestamp-component primitive-filter)
  ())

;;;;;;
;;; Member filter

(def component member-filter (member-component primitive-filter)
  ())











;;;;;;
;;; TODO:
#|
;; TODO: all predicates
(def (constant :test 'equalp) +filter-predicates+ '(equal like < <= > >= #+nil(between)))

          label (label (localized-slot-name slot))
          negate-command (make-instance 'command-component
                                        :icon (make-negated/ponated-icon negated)
                                        :action (make-action
                                                  (setf negated (not negated))
                                                  (setf (icon-of negate-command) (make-negated/ponated-icon negated))))
          predicate-command (make-instance 'command-component
                                           :icon (make-predicate-icon predicate)
                                           :action (make-action
                                                     (setf predicate (elt +filter-predicates+
                                                                          (mod (1+ (position predicate +filter-predicates+))
                                                                               (length +filter-predicates+))))
                                                     (setf (icon-of predicate-command) (make-predicate-icon predicate))))

(def function make-negated/ponated-icon (negated)
  (aprog1 (make-icon-component (if negated 'negated 'ponated))
    (setf (label-of it) nil)))

(def function make-predicate-icon (predicate)
  (aprog1 (make-icon-component predicate)
    (setf (label-of it) nil)))
|#