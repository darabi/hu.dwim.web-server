;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/filter

(def (component e) primitive/filter (primitive/presentation filter/abstract)
  ()
  (:documentation "A PRIMITIVE/FILTER filters the set of existing values of a primitive TYPE based on a filter criteria provided by the user."))

(def render-xhtml :before primitive/filter
  (ensure-client-state-sink -self-))

(def function make-update-use-in-filter-js (component)
  `js-inline(wui.field.update-use-in-filter ,(use-in-filter-id-of component) #t))

;;;;;;
;;; boolean/filter

(def (component e) boolean/filter (boolean/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* boolean boolean/filter)

(def render-xhtml boolean/filter
  (bind ((use-in-filter? (use-in-filter? -self-))
         (use-in-filter-id (use-in-filter-id-of -self-))
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    (if (eq (component-value-type-of -self-) 'boolean)
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

(def (component e) character/filter (character/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null character) character/filter)

(def render-xhtml character/filter
  (bind ((widget-id (generate-unique-string/frame "_stw")))
    (render-string-component -self- :id widget-id)
    `js(wui.field.setup-string-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; string/filter

(def (component e) string/filter (string/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null string) string/filter)

(def method collect-filter-predicates ((self string/filter))
  '(like equal less-than less-than-or-equal greater-than greater-than-or-equal))

(def render-xhtml string/filter
  (bind ((widget-id (generate-unique-string/frame "_stw")))
    (render-string-component -self- :id widget-id)
    `js(wui.field.setup-string-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; password/filter

(def (component e) password/filter (password/presentation string/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null password) password/filter)

;;;;;;
;;; symbol/filter

(def (component e) symbol/filter (symbol/presentation string/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null symbol) symbol/filter)

(def method print-component-value ((self symbol/filter))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? self)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (if (stringp component-value)
            component-value
            (qualified-symbol-name component-value)))))

;;;;;;
;;; keyword/filter

(def (component e) keyword/filter (keyword/presentation string/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null keyword) keyword/filter)

(def method print-component-value ((component keyword/filter))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (string+ ":" (if (stringp component-value)
                         component-value
                         (symbol-name component-value))))))

;;;;;;
;;; number/filter

(def (component e) number/filter (number/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null number) number/filter)

(def method collect-filter-predicates ((self number/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

(def render-xhtml number/filter
  (bind ((widget-id (generate-unique-string/frame "_stw")))
    (render-number-field-for-primitive-component -self- :id widget-id)
    `js(wui.field.setup-number-filter ,widget-id ,(use-in-filter-id-of -self-))))

;;;;;;
;;; integer/filter

(def (component e) integer/filter (integer/presentation number/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null integer) integer/filter)

;;;;;;
;;; float/filter

(def (component e) float/filter (float/presentation number/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null float) float/filter)

;;;;;;
;;; date/filter

(def (component e) date/filter (date/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null local-time:date) date/filter)

(def render-xhtml date/filter
  (render-date-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self date/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; time/filter

(def (component e) time/filter (time/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null local-time:time) time/filter)

(def render-xhtml time/filter
  (render-time-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self time/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; timestamp/filter

(def (component e) timestamp/filter (timestamp/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null local-time:timestamp) timestamp/filter)

(def render-xhtml timestamp/filter
  (render-timestamp-component -self- :on-change (make-update-use-in-filter-js -self-)))

(def method collect-filter-predicates ((self timestamp/filter))
  '(equal less-than less-than-or-equal greater-than greater-than-or-equal))

;;;;;;
;;; member/filter

(def (component e) member/filter (member/presentation primitive/filter)
  ())

(def finite-type-mapper *filter-type-mapping* 256 member/filter)

(def method collect-filter-predicates ((self member/filter))
  '(equal))

(def render-xhtml member/filter
  (render-member-component -self- :on-change (make-update-use-in-filter-js -self-)))

;;;;;;
;;; html/filter

(def (component e) html/filter (html/presentation string/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null html-text) html/filter)

;;;;;;
;;; inet-address/filter

(def (component e) inet-address/filter (inet-address/presentation primitive/filter)
  ())

(def subtype-mapper *filter-type-mapping* (or null iolib.sockets:inet-address) inet-address/filter)
