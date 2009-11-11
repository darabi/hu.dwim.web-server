;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/inspector

(def (component e) primitive/inspector (primitive/abstract inspector/abstract)
  ())

(def render-xhtml :before primitive/inspector
  (when (edited-component? -self-)
    (ensure-client-state-sink -self-)))

;;;;;;
;;; unbound/inspector

(def (component e) unbound/inspector (unbound/abstract primitive/inspector)
  ())

;;;;;;
;;; boolean/inspector

(def (component e) boolean/inspector (boolean/abstract primitive/inspector)
  ())

(def render-xhtml boolean/inspector
  (bind (((:read-only-slots the-type edited-component client-state-sink) -self-)
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    <div ,(when edited-component
            <input (:type "hidden"
                    :name ,(id-of client-state-sink)
                    :value ,(if component-value "true" "false"))>)
         ,(if (eq the-type 'boolean)
              (bind ((checked (when component-value "checked"))
                     (disabled (unless edited-component "disabled")))
                <input (:type "checkbox" :checked ,checked :disabled ,disabled)>)
              (bind ((disabled (unless edited-component "disabled")))
                <select (:disabled ,disabled)
                  ;; TODO: add error marker when no initform and default value is selected
                  ,(bind ((selected (unless has-component-value? "yes")))
                     <option (:selected ,selected)
                       ,#"value.nil">)
                  ,(bind ((selected (when (and has-component-value?
                                               component-value)
                                      "yes")))
                     <option (:selected ,selected)
                       ,#"boolean.true">)
                  ,(bind ((selected (when (and has-component-value?
                                               (not component-value))
                                      "yes")))
                     <option (:selected ,selected)
                      ,#"boolean.false">) >))>))

;;;;;;
;;; character/inspector

(def (component e) character/inspector (character/abstract primitive/inspector)
  ())

(def render-xhtml character/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-string-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; string/inspector

(def (component e) string/inspector (string/abstract primitive/inspector)
  ())

(def render-xhtml string/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-string-component -self-)
        `xml,(print-component-value -self-))))

(def render-text string/inspector
  (render-component (component-value-of -self-)))

;;;;;;
;;; ods export
(def render-ods string/inspector
    <text:p ,(component-value-of -self-)>)

;;;;;;
;;; password/inspector

(def (component e) password/inspector (password/abstract string/inspector)
  ())

(def method print-component-value ((component password/inspector))
  (if (edited-component? component)
      (call-next-method)
      "**********"))

;;;;;;
;;; symbol/inspector

(def (component e) symbol/inspector (symbol/abstract string/inspector)
  ())

;;;;;;
;;; keyword/inspector

(def (component e) keyword/inspector (keyword/abstract string/inspector)
  ())

;;;;;;
;;; number/inspector

(def (component e) number/inspector (number/abstract primitive/inspector)
  ())

(def render-xhtml number/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-number-field-for-primitive-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; integer/inspector

(def (component e) integer/inspector (integer/abstract number/inspector)
  ())

;;;;;;
;;; float/inspector

(def (component e) float/inspector (float/abstract number/inspector)
  ())

;;;;;;
;;; date/inspector

(def (component e) date/inspector (date/abstract primitive/inspector)
  ())

(def render-xhtml date/inspector
  (if (edited-component? -self-)
      (render-date-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; time/inspector

(def (component e) time/inspector (time/abstract primitive/inspector)
  ())

(def render-xhtml time/inspector
  (if (edited-component? -self-)
      (render-time-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; timestamp/inspector

(def (component e) timestamp/inspector (timestamp/abstract primitive/inspector)
  ())

(def render-xhtml timestamp/inspector
  (if (edited-component? -self-)
      (render-timestamp-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; member/inspector

(def (component e) member/inspector (member/abstract primitive/inspector)
  ())

(def render-xhtml member/inspector
  (if (edited-component? -self-)
      (render-member-component -self-)
      (bind ((icon (find-member-component-value-icon -self-)))
        (if icon
            (render-icon :icon icon :label (print-component-value -self-) :tooltip nil)
            `xml,(print-component-value -self-)))))

;;;;;;
;;; html/inspector

(def (component e) html/inspector (html/abstract primitive/inspector)
  ())

(def render-xhtml html/inspector
  (if (edited-component? -self-)
      (render-html-component -self-)
      (emit-html-component-value -self-)))

;;;;;;
;;; ip-address/inspector

(def (component e) ip-address/inspector (ip-address/abstract primitive/inspector)
  ())

(def method print-component-value ((self ip-address/inspector))
  (if (edited-component? self)
      (call-next-method)
      (bind ((value (component-value-of self)))
        (iolib:address-to-string
         (etypecase value
           (iolib:inet-address                    value)
           ((simple-array (unsigned-byte 8) (4))  (make-instance 'iolib:ipv4-address :name value))
           ((simple-array (unsigned-byte 16) (8)) (make-instance 'iolib:ipv6-address :name value)))))))

(def render-xhtml ip-address/inspector
  (if (edited-component? -self-)
      (call-next-method)
      <span (:class "ip-address")
        ,(print-component-value -self-)>))

;;;;;;
;;; file/inspector

(def (component e) file/inspector (file/abstract primitive/inspector)
  ((upload-command :type component)
   (download-command :type component)
   (directory "/tmp/")
   (file-name)
   (url-prefix "static/")))

(def refresh-component file/inspector
  (bind (((:slots upload-command download-command directory file-name url-prefix) -self-)
         ((:values class instance slot) (extract-primitive-component-place -self-)))
    (setf upload-command (command/widget ()
                           (icon upload-file)
                           (make-action
                             (upload-file -self-))))
    (setf download-command (command/widget (:delayed-content #t
                                            :path (download-file-name -self- class instance slot))
                             (icon download-file)
                             (make-action
                               (download-file -self-))))))

(def render-xhtml file/inspector
  (if (edited-component? -self-)
      (render-component (upload-command-of -self-))
      (render-component (download-command-of -self-))))

(def (layered-function e) upload-file (component))

(def (layered-function e) download-file (component))
