;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/inspector

(def (component e) primitive/inspector (primitive/presentation t/inspector)
  ()
  (:documentation "A PRIMITIVE/INSPECTOR displays or edits existing values of primitive TYPEs."))

(def render-xhtml :before primitive/inspector
  (when (edited-component? -self-)
    (ensure-client-state-sink -self-)))

;;;;;;
;;; unbound/inspector

(def (component e) unbound/inspector (unbound/presentation primitive/inspector)
  ())

;;;;;;
;;; null/inspector

(def (component e) null/inspector (null/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* null null/inspector)

;;;;;;
;;; boolean/inspector

(def (component e) boolean/inspector (boolean/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* boolean boolean/inspector)

(def function render-boolean-component (component &key (component-value-transformer #'identity))
  (bind (((:read-only-slots component-value-type edited-component client-state-sink) component)
         (has-component-value? (slot-boundp component 'component-value))
         (component-value (funcall component-value-transformer
                                   (when has-component-value?
                                     (component-value-of component)))))
    <div ,(when edited-component
            <input (:type "hidden"
                    :name ,(id-of client-state-sink)
                    :value ,(if component-value "true" "false"))>)
         ,(if (symbolp component-value-type)
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

(def render-xhtml boolean/inspector
  (render-boolean-component -self-))


;;;;;;
;;; bit/inspector

(def (component e) bit/inspector (bit/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null bit) bit/inspector)

(def render-xhtml bit/inspector
  (render-boolean-component -self- :component-value-transformer [not (zerop !1)]))

;;;;;;
;;; character/inspector

(def (component e) character/inspector (character/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null character) character/inspector)

(def render-xhtml character/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-string-component -self-)
        (render-component-value -self-))))

;;;;;;
;;; string/inspector

(def (component e) string/inspector (string/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null string) string/inspector)

(def render-xhtml string/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-string-component -self-)
        (render-component-value -self-))))

;;;;;;
;;; password/inspector

(def (component e) password/inspector (password/presentation string/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null password) password/inspector)

(def method print-component-value ((component password/inspector))
  (if (edited-component? component)
      (call-next-method)
      "**********"))

;;;;;;
;;; symbol/inspector

(def (component e) symbol/inspector (symbol/presentation string/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null symbol) symbol/inspector)

;;;;;;
;;; keyword/inspector

(def (component e) keyword/inspector (keyword/presentation string/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null keyword) keyword/inspector)

;;;;;;
;;; number/inspector

(def (component e) number/inspector (number/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null number) number/inspector)

(def render-xhtml number/inspector
  (bind (((:read-only-slots edited-component) -self-))
    (if edited-component
        (render-number-field-for-primitive-component -self-)
        (render-component-value -self-))))

;;;;;;
;;; real/inspector

(def (component e) real/inspector (real/presentation number/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null real) real/inspector)

;;;;;;
;;; complex/inspector

(def (component e) complex/inspector (complex/presentation number/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null complex) complex/inspector)

;;;;;;
;;; rational/inspector

(def (component e) rational/inspector (rational/presentation real/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null rational) rational/inspector)

;;;;;;
;;; integer/inspector

(def (component e) integer/inspector (integer/presentation rational/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null integer) integer/inspector)

;;;;;;
;;; float/inspector

(def (component e) float/inspector (float/presentation real/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null float) float/inspector)

;;;;;;
;;; date/inspector

(def (component e) date/inspector (date/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null local-time:date) date/inspector)

(def render-xhtml date/inspector
  (if (edited-component? -self-)
      (render-date-component -self-)
      (render-component-value -self-)))

;;;;;;
;;; time-of-day/inspector

(def (component e) time-of-day/inspector (time-of-day/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null local-time:time-of-day) time-of-day/inspector)

(def render-xhtml time-of-day/inspector
  (if (edited-component? -self-)
      (render-time-component -self-)
      (render-component-value -self-)))

;;;;;;
;;; timestamp/inspector

(def (component e) timestamp/inspector (timestamp/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null local-time:timestamp) timestamp/inspector)

(def render-xhtml timestamp/inspector
  (if (edited-component? -self-)
      (render-timestamp-component -self-)
      (render-component-value -self-)))

;;;;;;
;;; member/inspector

(def (component e) member/inspector (member/presentation primitive/inspector)
  ())

(def finite-type-mapper *inspector-type-mapping* 256 member/inspector)

(def render-xhtml member/inspector
  (if (edited-component? -self-)
      (render-member-component -self-)
      (bind ((icon (find-icon/member-component-value -self-)))
        (if icon
            (render-icon :icon icon :label (print-component-value -self-) :tooltip nil)
            (render-component-value -self-)))))

;;;;;;
;;; html/inspector

(def (component e) html/inspector (html/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null html-text) html/inspector)

(def render-xhtml html/inspector
  (if (edited-component? -self-)
      (render-html-component -self-)
      (emit-html-component-value -self-)))

;;;;;;
;;; inet-address/inspector

(def (component e) inet-address/inspector (inet-address/presentation primitive/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null iolib.sockets:inet-address) inet-address/inspector)

(def method print-component-value ((self inet-address/inspector))
  (if (edited-component? self)
      (call-next-method)
      (bind ((value (component-value-of self)))
        (if value
            (iolib:address-to-string
             (etypecase value
               (iolib:inet-address value)
               ((simple-array (unsigned-byte 8) (4)) (make-instance 'iolib:ipv4-address :name value))
               ((simple-array (unsigned-byte 16) (8)) (make-instance 'iolib:ipv6-address :name value))))
            ""))))

(def render-xhtml inet-address/inspector
  (if (edited-component? -self-)
      (call-next-layered-method)
      (print-component-value -self-)))

;;;;;;
;;; file/inspector

(def (component e) file/inspector (file/presentation primitive/inspector)
  ((upload-command :type component)
   (download-command :type component)
   (directory "/tmp/")
   (file-name)
   (url-prefix "static/")))

(def refresh-component file/inspector
  (bind (((:slots upload-command download-command directory file-name url-prefix) -self-)
         ((:values class instance slot) (extract-primitive-component-place -self-)))
    (setf upload-command (command/widget ()
                           (icon/widget upload-file)
                           (make-action
                             (upload-file -self-))))
    (setf download-command (command/widget (:delayed-content #t
                                            :path (download-file-name -self- class instance slot))
                             (icon/widget download-file)
                             (make-action
                               (download-file -self-))))))

(def render-xhtml file/inspector
  (if (edited-component? -self-)
      (render-component (upload-command-of -self-))
      (render-component (download-command-of -self-))))

(def (layered-function e) upload-file (component))

(def (layered-function e) download-file (component))
