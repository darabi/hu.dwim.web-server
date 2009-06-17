;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place filter

(def (component ea) place-filter (place-component filter/abstract)
  ((name nil)
   (the-type)
   (use-in-filter #f :type boolean)
   (use-in-filter-id)
   (negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def method make-place-component-content ((self place-filter))
  (make-filter (the-type-of self)))

(def method use-in-filter-p ((self component))
  (use-in-filter-p (parent-component-of self)))

(def method use-in-filter-id-of ((self component))
  (use-in-filter-id-of (parent-component-of self)))

(def generic collect-possible-filter-predicates (component)
  (:method ((self primitive-filter))
    nil))

(def function localize-predicate (predicate)
  (lookup-resource (concatenate-string "predicate." (symbol-name predicate))))

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
    <td ,(render-checkbox-field (use-in-filter-p self)
                                :id id
                                :value-sink (lambda (value) (setf (use-in-filter-p self) value))
                                :checked-class "icon use-in-filter"
                                :unchecked-class "icon ignore-in-filter")>))

(def render-xhtml place-filter
  (render-filter-predicate -self-)
  (render-use-in-filter-marker -self-)
  <td ,(call-next-method)>)


(def icon equal)
(def resources hu
  (icon-label.equal "Egyenlő")
  (icon-tooltip.equal "Ellenőrzes egyenlőségre"))
(def resources en
  (icon-label.equal "Equal")
  (icon-tooltip.equal "Compare for equality"))

(def icon like)
(def resources hu
  (icon-label.like "Hasonló")
  (icon-tooltip.like "Ellenőrzes hasonlóságra"))
(def resources en
  (icon-label.like "Like")
  (icon-tooltip.like "Compare for like"))

(def icon <)
(def resources hu
  (icon-label.< "Kisebb")
  (icon-tooltip.< "Ellenőrzes kisebbre"))
(def resources en
  (icon-label.< "Less")
  (icon-tooltip.< "Compare for less then"))

(def icon <=)
(def resources hu
  (icon-label.<= "Kisebb vagy egyenlő")
  (icon-tooltip.<= "Ellenőrzes kisebbre vagy egyenlőre"))
(def resources en
  (icon-label.<= "Less or equal")
  (icon-tooltip.<= "Compare for less than or equal"))

(def icon >)
(def resources hu
  (icon-label.> "Nagyobb")
  (icon-tooltip.> "Ellenőrzes nagyobbra"))
(def resources en
  (icon-label.> "Greater")
  (icon-tooltip.> "Compare for greater then"))

(def icon >=)
(def resources hu
  (icon-label.>= "Nagyobb vagy egyenlő")
  (icon-tooltip.>= "Ellenőrzes nagyobb vagy egyenlőre"))
(def resources en
  (icon-label.>= "Greater or equal")
  (icon-tooltip.>= "Compare for greater than or equal"))

(def icon negated)
(def resources hu
  (icon-label.negated "Negált")
  (icon-tooltip.negated "Negált feltétel"))
(def resources en
  (icon-label.negated "Negated")
  (icon-tooltip.negated "Negate condition"))

(def icon ponated)
(def resources hu
  (icon-label.ponated "Ponált")
  (icon-tooltip.ponated "Ponált feltétel"))
(def resources en
  (icon-label.ponated "Ponated")
  (icon-tooltip.ponated "Ponate condition"))

(def resources hu
  (predicate.= "Egyenlő")
  (predicate.~ "Hasonló")
  (predicate.< "Kisebb")
  (predicate.≤ "Kisebb vagy egyenlő")
  (predicate.> "Nagyobb")
  (predicate.≥ "Nagyobb vagy egyenlő"))

(def resources en
  (predicate.= "Equal")
  (predicate.~ "Like")
  (predicate.< "Smaller than")
  (predicate.≤ "Smaller than or equal")
  (predicate.> "Greater than")
  (predicate.≥ "Greater than or equal"))
