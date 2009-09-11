;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place filter

(def (component e) place-filter (place-component filter/abstract)
  ((name nil)
   (the-type)
   (use-in-filter #f :type boolean)
   (use-in-filter-id)
   (negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def method make-place-component-content ((self place-filter))
  (make-filter (the-type-of self)))

(def method use-in-filter? ((self component))
  (use-in-filter? (parent-component-of self)))

(def method use-in-filter-id-of ((self component))
  (use-in-filter-id-of (parent-component-of self)))

(def generic collect-possible-filter-predicates (component)
  (:method ((self primitive-filter))
    nil))

(def function localize-predicate (predicate)
  (lookup-resource (string+ "predicate." (symbol-name predicate))))

(def function predicate-class (predicate)
  (ecase predicate
    (= "predicate-equal")
    (~ "predicate-like")
    (< "predicate-less-than")
    (≤ "predicate-less-than-or-equal")
    (> "predicate-greater-than")
    (≥ "predicate-greater-than-or-equal")))

(def generic predicate-function (component class predicate)
  (:method ((component place-filter) (class standard-class) predicate)
    (predicate-function (content-of component) class predicate))

  (:method ((component component) (class standard-class) (predicate null))
    'equal)

  (:method ((component component) (class standard-class) (predicate (eql '=)))
    'equal)

  (:method ((component string-component) (class standard-class) (predicate (eql '~)))
    (bind ((scanner nil)
           (previous-regexp nil))
      (lambda (value regexp)
        (unless (equal regexp previous-regexp)
          (setf scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode #t)))
        (and (stringp value)
             (cl-ppcre:do-matches (match-start match-end scanner value nil)
               (return #t))))))

  (:method ((component string-component) (class standard-class) (predicate (eql '<)))
    'string<)

  (:method ((component string-component) (class standard-class) (predicate (eql '≤)))
    'string<=)

  (:method ((component string-component) (class standard-class) (predicate (eql '>)))
    'string>)

  (:method ((component string-component) (class standard-class) (predicate (eql '≥)))
    'string>=)

  (:method ((component number-component) (class standard-class) (predicate (eql '<)))
    '<)

  (:method ((component number-component) (class standard-class) (predicate (eql '≤)))
    '<=)

  (:method ((component number-component) (class standard-class) (predicate (eql '>)))
    '>)

  (:method ((component number-component) (class standard-class) (predicate (eql '≥)))
    '>=))

(def method collect-possible-filter-predicates ((self place-filter))
  (collect-possible-filter-predicates (content-of self)))

(def function render-filter-predicate (self)
  (bind (((:slots negated selected-predicate) self)
         (possible-predicates (collect-possible-filter-predicates self)))
    (if possible-predicates
        (progn
          (unless selected-predicate
            (setf selected-predicate (first possible-predicates)))
          <td ,(render-checkbox-field negated
                                      :value-sink (lambda (value) (setf negated value))
                                      :checked-class "icon negated-icon"
                                      :unchecked-class "icon ponated-icon")>
          <td ,(if (length= 1 possible-predicates)
                   <div (:class ,(predicate-class (first possible-predicates)))>
                   (render-popup-menu-select-field (localize-predicate selected-predicate)
                                                   (mapcar #'localize-predicate possible-predicates)
                                                   :value-sink (lambda (value)
                                                                 (setf selected-predicate (find value possible-predicates :key #'localize-predicate :test #'string=)))
                                                   :classes (mapcar #'predicate-class possible-predicates)))>)
        <td (:colspan 2)>)))

(def function render-use-in-filter-marker (self)
  (bind ((id (generate-frame-unique-string)))
    (setf (use-in-filter-id-of self) id)
    <td ,(render-checkbox-field (use-in-filter? self)
                                :id id
                                :value-sink (lambda (value) (setf (use-in-filter? self) value))
                                :checked-class "icon use-in-filter"
                                :unchecked-class "icon ignore-in-filter")>))

(def render-xhtml place-filter
  (render-filter-predicate -self-)
  (render-use-in-filter-marker -self-)
  <td ,(call-next-method)>)

;;;;;;
;;; Icon

(def (icon e) equal)

(def (icon e) like)

(def (icon e) <)

(def (icon e) <=)

(def (icon e) >)

(def (icon e) >=)

(def (icon e) negated)

(def (icon e) ponated)
