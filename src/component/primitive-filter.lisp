;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ())

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

(def component string-filter (string-component primitive-filter)
  ((component-value nil)))

(def method collect-possible-filter-predicates ((self string-filter))
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

(def component number-filter (number-component primitive-filter)
  ())

(def method collect-possible-filter-predicates ((self number-filter))
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

(def component member-filter (member-component primitive-filter)
  ())

(def method collect-possible-filter-predicates ((self member-filter))
  '(=))

(def render member-filter ()
  (ensure-client-state-sink -self-)
  (render-member-component -self- :on-change (delay `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of -self-) #t))))
