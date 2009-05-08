;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ())

(def function make-update-use-in-filter-js (component)
  `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of component) #t))

;;;;;;
;;; T filter

(def component t-filter (t-component primitive-filter)
  ())

(def render-xhtml t-filter
  (ensure-client-state-sink -self-)
  (render-t-component -self-))

;;;;;;
;;; Boolean filter

(def component boolean-filter (boolean-component primitive-filter)
  ())

(def render-xhtml boolean-filter
  (ensure-client-state-sink -self-)
  (bind ((use-in-filter? (use-in-filter-p -self-))
         (use-in-filter-id (use-in-filter-id-of -self-))
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    (if (eq (the-type-of -self-) 'boolean)
        (render-checkbox-field component-value
                               :name (client-state-sink-of -self-)
                               :on-change `js-inline(wui.field.update-use-in-filter ,use-in-filter-id #t))
        <select (:name ,(id-of (client-state-sink-of -self-))
                 :onChange `js-inline(wui.field.update-use-in-filter ,use-in-filter-id #t))
          ,(bind ((selected (when (and use-in-filter?
                                       (not has-component-value?))
                              "yes")))
                 <option (:value "" :selected ,selected)
                         ,#"value.nil">)
          ,(bind ((selected (when (and use-in-filter?
                                       has-component-value?
                                       component-value)
                              "yes")))
                 <option (:value "true" :selected ,selected)
                         ,#"boolean.true">)
          ,(bind ((selected (when (and use-in-filter?
                                       has-component-value?
                                       (not component-value))
                              "yes")))
                 <option (:value "false" :selected ,selected)
                         ,#"boolean.false">)>)))

;;;;;;
;;; String filter

(def component string-filter (string-component primitive-filter)
  ((component-value nil)))

(def method collect-possible-filter-predicates ((self string-filter))
  '(= ~ < ≤ > ≥))

(def render-xhtml string-filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-string-component -self- :id widget-id)
    `js(wui.field.setup-string-filter ,widget-id ,(use-in-filter-id-of -self-))))

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

(def render-xhtml number-filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-number-field-for-primitive-component -self- :id widget-id)
    `js(wui.field.setup-number-filter ,widget-id ,(use-in-filter-id-of -self-))))

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

(def render-xhtml date-filter
  (ensure-client-state-sink -self-)
  (render-date-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self date-filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Time filter

(def component time-filter (time-component primitive-filter)
  ())

(def render-xhtml time-filter
  (ensure-client-state-sink -self-)
  (render-time-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self time-filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Timestamp filter

(def component timestamp-filter (timestamp-component primitive-filter)
  ())

(def render-xhtml timestamp-filter
  (ensure-client-state-sink -self-)
  (render-timestamp-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self timestamp-filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Member filter

(def component member-filter (member-component primitive-filter)
  ())

(def method collect-possible-filter-predicates ((self member-filter))
  '(=))

(def render-xhtml member-filter
  (ensure-client-state-sink -self-)
  (render-member-component -self- :on-change (make-update-use-in-filter-js -self-)))

;;;;;;
;;; HTML filter

(def component html-filter (html-component string-filter)
  ())

;;;;;;
;;; IP address filter

(def component ip-address-filter (ip-address-component primitive-filter)
  ())
