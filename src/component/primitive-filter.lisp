;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ((use-in-filter #f :type boolean)
   (use-in-filter-id)))

(def generic render-filter-predicate (component)
  (:method ((self component))
    <td>
    <td>))

(def generic render-use-in-filter-marker (component)
  (:method ((self component))
    (bind ((id (generate-frame-unique-string)))
      (setf (use-in-filter-id-of self) id)
      <td ,(render-checkbox-field (use-in-filter-p self)
                                  :id id
                                  :value-sink (lambda (value) (setf (use-in-filter-p self) value))
                                  :checked-image "static/wui/icons/20x20/checkmark.png"
                                  :unchecked-image "static/wui/icons/20x20/checkmark.png"
                                  :checked-class "use-in-filter"
                                  :unchecked-class "ignore-in-filter")>)))

(def render :around primitive-filter ()
  (render-filter-predicate -self-)
  (render-use-in-filter-marker -self-)
  <td ,(call-next-method)>)

;;;;;;
;;; Predicate mixin

(def component filter-with-predicate-mixin ()
  ((negated #f :type boolean)
   (selected-predicate nil :type symbol)))

(def generic collect-possible-predicates (component))

(def function localize-predicate (predicate)
  (lookup-resource (concatenate-string "predicate." (symbol-name predicate)) nil))

(defresources en
  (predicate.= "Equal")
  (predicate.~ "Like")
  (predicate.< "Smaller than")
  (predicate.≤ "Smaller than or equal")
  (predicate.> "Greater than")
  (predicate.≥ "Greater than or equal"))

(defresources hu
  (predicate.= "Egyenlő")
  (predicate.~ "Hasonló")
  (predicate.< "Kisebb")
  (predicate.≤ "Kisebb vagy egyenlő")
  (predicate.> "Nagyobb")
  (predicate.≥ "Nagyobb vagy egyenlő"))

(def function predicate-class (predicate)
  (ecase predicate
    (= "predicate-equal")
    (~ "predicate-like")
    (< "predicate-less-than")
    (≤ "predicate-less-than-or-equal")
    (> "predicate-greater-than")
    (≥ "predicate-greater-than-or-equal")))

(def generic predicate-function (component class predicate)
  (:method ((component component) (class standard-class) (predicate (eql '=)))
    'equal)

  (:method ((component string-component) (class standard-class) (predicate (eql '~)))
    (lambda (string-1 string-2)
      (cl-ppcre:all-matches string-2 (concatenate-string ".*" string-1 ".*"))))

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

(def method render-filter-predicate ((self filter-with-predicate-mixin))
  (bind (((:slots negated selected-predicate) self)
         (possible-predicates (collect-possible-predicates self)))
    (unless selected-predicate
      (setf selected-predicate (first possible-predicates)))
    <td ,(render-checkbox-field negated
                                :value-sink (lambda (value) (setf negated value))
                                :checked-image "static/wui/icons/20x20/thumb-down.png"
                                :unchecked-image "static/wui/icons/20x20/thumb-up.png")>
    <td ,(unless (length= 1 possible-predicates)
           (render-popup-menu-select-field (localize-predicate selected-predicate)
                                           (mapcar #'localize-predicate possible-predicates)
                                           :value-sink (lambda (value)
                                                         (setf (selected-predicate-of self)
                                                               (find value possible-predicates :key #'localize-predicate :test #'string=)))
                                           :classes (mapcar #'predicate-class possible-predicates))) >))

;;;;;;
;;; T filter

(def component t-filter (t-component primitive-filter)
  ())

(def render t-filter ()
  (render-t-component -self-))

;;;;;;
;;; Boolean filter

(def component boolean-filter (boolean-component primitive-filter)
  ())

(def render boolean-filter ()
  (ensure-client-state-sink -self-)
  (bind ((use-in-filter? (use-in-filter-p -self-))
         (use-in-filter-id (use-in-filter-id-of -self-))
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    (if (eq (the-type-of -self-) 'boolean)
        (render-checkbox-field component-value
                               :name (client-state-sink-of -self-)
                               :on-change (delay `js-inline(wui.field.update-use-in-filter ,use-in-filter-id #t)))
        <select (:name ,(id-of (client-state-sink-of -self-))
                 :onchange `js-inline(wui.field.update-use-in-filter ,use-in-filter-id #t))
          <option (:value ""
                   ,(when (and use-in-filter?
                               (not has-component-value?))
                      (make-xml-attribute "selected" "yes")))
            ,#"value.nil">
          <option (:value "true"
                   ,(when (and use-in-filter?
                               has-component-value?
                               component-value)
                      (make-xml-attribute "selected" "yes")))
            ,#"boolean.true">
          <option (:value "false"
                   ,(when (and use-in-filter?
                               has-component-value?
                               (not component-value))
                      (make-xml-attribute "selected" "yes")))
            ,#"boolean.false">>)))

;;;;;;
;;; String filter

(def component string-filter (string-component primitive-filter filter-with-predicate-mixin)
  ((component-value nil)))

(def method collect-possible-predicates ((self string-filter))
  '(= ~ < ≤ > ≥))

(def render string-filter ()
  (ensure-client-state-sink -self-)
  (render-string-component -self- :on-change (delay `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of -self-) (!= "" this.value)))))

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

(def component number-filter (number-component primitive-filter filter-with-predicate-mixin)
  ())

(def method collect-possible-predicates ((self number-filter))
  '(= < ≤ > ≥))

(def render number-filter ()
  (ensure-client-state-sink -self-)
  (render-number-component -self- :on-change (delay `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of -self-) (!= "" this.value)))))

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

(def component member-filter (member-component primitive-filter filter-with-predicate-mixin)
  ())

(def method collect-possible-predicates ((self member-filter))
  '(=))

(def render member-filter ()
  (ensure-client-state-sink -self-)
  (render-member-component -self- :on-change (delay `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of -self-) #t))))
