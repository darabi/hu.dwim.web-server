;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/filter

(def (component e) primitive/filter (primitive/abstract filter/abstract)
  ())

(def function make-update-use-in-filter-js (component)
  `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of component) #t))

;;;;;;
;;; boolean/filter

(def (component e) boolean/filter (boolean/abstract primitive/filter)
  ())

(def render-xhtml boolean/filter
  (ensure-client-state-sink -self-)
  (bind ((use-in-filter? (use-in-filter? -self-))
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
;;; character/filter

(def (component e) character/filter (character/abstract primitive/filter)
  ())

;;;;;;
;;; string/filter

(def (component e) string/filter (string/abstract primitive/filter)
  ((component-value nil)))

(def method collect-filter-predicates ((self string/filter))
  '(like equal less-than less-than-or-equal greater-than greater-than-or-equal))

(def render-xhtml string/filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-string-component -self- :id widget-id)
    `js(wui.field.setup-string-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; password/filter

(def (component e) password/filter (password/abstract string/filter)
  ())

;;;;;;
;;; symbol/filter

(def (component e) symbol/filter (symbol/abstract string/filter)
  ())

(def method print-component-value ((self symbol/filter))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p self)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (if (stringp component-value)
            component-value
            (qualified-symbol-name component-value)))))

;;;;;;
;;; number/filter

(def (component e) number/filter (number/abstract primitive/filter)
  ())

(def method collect-filter-predicates ((self number/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

(def render-xhtml number/filter
  (ensure-client-state-sink -self-)
  (bind ((widget-id (generate-frame-unique-string "_stw")))
    (render-number-field-for-primitive-component -self- :id widget-id)
    `js(wui.field.setup-number-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; integer/filter

(def (component e) integer/filter (integer/abstract number/filter)
  ())

;;;;;;
;;; float/filter

(def (component e) float/filter (float/abstract number/filter)
  ())

;;;;;;
;;; date/filter

(def (component e) date/filter (date/abstract primitive/filter)
  ())

(def render-xhtml date/filter
  (ensure-client-state-sink -self-)
  (render-date-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self date/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; time/filter

(def (component e) time/filter (time/abstract primitive/filter)
  ())

(def render-xhtml time/filter
  (ensure-client-state-sink -self-)
  (render-time-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self time/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; timestamp/filter

(def (component e) timestamp/filter (timestamp/abstract primitive/filter)
  ())

(def render-xhtml timestamp/filter
  (ensure-client-state-sink -self-)
  (render-timestamp-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self timestamp/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; member/filter

(def (component e) member/filter (member/abstract primitive/filter)
  ())

(def method collect-filter-predicates ((self member/filter))
  '(equal))

(def render-xhtml member/filter
  (ensure-client-state-sink -self-)
  (render-member-component -self- :on-change (make-update-use-in-filter-js -self-)))

;;;;;;
;;; html/filter

(def (component e) html/filter (html/abstract string/filter)
  ())

;;;;;;
;;; ip-address/filter

(def (component e) ip-address/filter (ip-address/abstract primitive/filter)
  ())
