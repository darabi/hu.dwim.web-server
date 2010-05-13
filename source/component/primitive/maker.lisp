;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/maker

(def (component e) primitive/maker (primitive/presentation t/maker)
  ((initform)
   (use-initform :type boolean))
  (:documentation "A PRIMITIVE/MAKER makers new values of primitive TYPEs."))

(def constructor primitive/maker ()
  (setf (use-initform? -self-)
        (slot-boundp -self- 'initform)))

(def render-xhtml :before primitive/maker
  (ensure-client-state-sink -self-))

(def function render-initform (component)
  (when (slot-boundp component 'initform)
    <span ,#"value.defaults-to" ,(princ-to-string (initform-of component))>))

;;;;;;
;;; unbound/maker

(def (component e) unbound/maker (unbound/presentation primitive/maker)
  ())

;;;;;;
;;; null/maker

(def (component e) null/maker (null/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* null null/maker)

;;;;;;
;;; boolean/maker

(def (component e) boolean/maker (boolean/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* boolean boolean/maker)

(def render-xhtml boolean/maker
  (bind (((:read-only-slots component-value-type) -self-)
         (has-initform? (slot-boundp -self- 'initform))
         (initform (when has-initform?
                     (initform-of -self-)))
         (constant-initform? (member initform '(#f #t))))
    (if (and (eq component-value-type 'boolean)
             has-initform?
             constant-initform?)
        (bind ((checked (when (and has-initform?
                                   (eq initform #t))
                          "checked")))
          <input (:type "checkbox" :checked ,checked)>)
        <select ()
          ;; TODO: add error marker when no initform and default value is selected
          ,(bind ((selected (unless (and has-initform?
                                         constant-initform?)
                              "yes")))
                 <option (:selected ,selected)
                   ,(cond (has-initform? #"value.default")
                          ((eq component-value-type 'boolean) "")
                          (t #"value.nil"))>)
          ,(bind ((selected (when (and has-initform?
                                       (eq initform #t))
                              "yes")))
                 <option (:selected ,selected)
                   ,#"boolean.true">)
          ,(bind ((selected (when (and has-initform?
                                       (eq initform #f))
                              "yes")))
                 <option (:selected ,selected)
                   ,#"boolean.false">) >)))

;;;;;;
;;; character/maker

(def (component e) character/maker (character/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null character) character/maker)

(def render-xhtml character/maker
  (render-string-component -self-))

;;;;;;
;;; string/maker

(def (component e) string/maker (string/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null string) string/maker)

(def render-xhtml string/maker
  (render-string-component -self-))

;;;;;;
;;; password/maker

(def (component e) password/maker (password/presentation string/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null password) password/maker)

;;;;;;
;;; symbol/maker

(def (component e) symbol/maker (symbol/presentation string/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null symbol) symbol/maker)

;;;;;;
;;; keyword/maker

(def (component e) keyword/maker (keyword/presentation string/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null keyword) keyword/maker)

;;;;;;
;;; number/maker

(def (component e) number/maker (number/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null number) number/maker)

(def render-xhtml number/maker
  (render-number-field-for-primitive-component -self-))

;;;;;;
;;; integer/maker

(def (component e) integer/maker (integer/presentation number/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null integer) integer/maker)

;;;;;;
;;; float/maker

(def (component e) float/maker (float/presentation number/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null float) float/maker)

;;;;;;
;;; date/maker

(def (component e) date/maker (date/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null local-time:date) date/maker)

(def render-xhtml date/maker
  (render-date-component -self-))

;;;;;;
;;; time-of-day/maker

(def (component e) time-of-day/maker (time-of-day/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null local-time:time-of-day) time-of-day/maker)

(def render-xhtml time-of-day/maker
  (render-time-component -self-))

;;;;;;
;;; timestamp/maker

(def (component e) timestamp/maker (timestamp/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null local-time:timestamp) timestamp/maker)

(def render-xhtml timestamp/maker
  (render-timestamp-component -self-))

;;;;;;
;;; member/maker

(def (component e) member/maker (member/presentation primitive/maker)
  ())

(def finite-type-mapper *maker-type-mapping* 256 member/maker)

(def render-xhtml member/maker
  (render-member-component -self-))

;;;;;;
;;; html/maker

(def (component e) html/maker (html/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null html-text) html/maker)

(def render-xhtml html/maker
  (render-html-component -self-))

;;;;;;
;;; inet-address/maker

(def (component e) inet-address/maker (inet-address/presentation primitive/maker)
  ())

(def subtype-mapper *maker-type-mapping* (or null iolib.sockets:inet-address) inet-address/maker)
