;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def (component ea) primitive/filter (primitive/abstract filter/abstract)
  ())

(def function make-update-use-in-filter-js (component)
  `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of component) #t))

;;;;;;
;;; T filter

(def (component ea) t/filter (t/abstract primitive/filter)
  ())

(def render-xhtml t/filter
  (ensure-client-state-sink -self-)
  (render-t-component -self-))

;;;;;;
;;; Boolean filter

(def (component ea) boolean/filter (boolean/abstract primitive/filter)
  ())

(def render-xhtml boolean/filter
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

(def (component ea) string/filter (string/abstract primitive/filter)
  ((component-value nil)))

(def method collect-possible-filter-predicates ((self string/filter))
  '(~ = < ≤ > ≥))

(def render-xhtml string/filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-string-component -self- :id widget-id)
    `js(wui.field.setup-string-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; Password filter

(def (component ea) password/filter (password/abstract string/filter)
  ())

;;;;;;
;;; Symbol filter

(def (component ea) symbol/filter (symbol/abstract string/filter)
  ())

;;;;;;
;;; Number filter

(def (component ea) number/filter (number/abstract primitive/filter)
  ())

(def method collect-possible-filter-predicates ((self number/filter))
  '(= < ≤ > ≥))

(def render-xhtml number/filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-number-field-for-primitive-component -self- :id widget-id)
    `js(wui.field.setup-number-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; Integer filter

(def (component ea) integer/filter (integer/abstract number/filter)
  ())

;;;;;;
;;; Float filter

(def (component ea) float/filter (float/abstract number/filter)
  ())

;;;;;;
;;; Date filter

(def (component ea) date/filter (date/abstract primitive/filter)
  ())

(def render-xhtml date/filter
  (ensure-client-state-sink -self-)
  (render-date-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self date/filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Time filter

(def (component ea) time/filter (time/abstract primitive/filter)
  ())

(def render-xhtml time/filter
  (ensure-client-state-sink -self-)
  (render-time-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self time/filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Timestamp filter

(def (component ea) timestamp/filter (timestamp/abstract primitive/filter)
  ())

(def render-xhtml timestamp/filter
  (ensure-client-state-sink -self-)
  (render-timestamp-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-possible-filter-predicates ((self timestamp/filter))
  '(= < ≤ > ≥))

;;;;;;
;;; Member filter

(def (component ea) member/filter (member/abstract primitive/filter)
  ())

(def method collect-possible-filter-predicates ((self member/filter))
  '(=))

(def render-xhtml member/filter
  (ensure-client-state-sink -self-)
  (render-member-component -self- :on-change (make-update-use-in-filter-js -self-)))

;;;;;;
;;; HTML filter

(def (component ea) html/filter (html/abstract string/filter)
  ())

;;;;;;
;;; IP address filter

(def (component ea) ip-address/filter (ip-address/abstract primitive/filter)
  ())
