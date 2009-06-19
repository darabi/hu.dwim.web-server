;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive inspector

(def (component e) primitive-inspector (primitive/abstract inspector/abstract editable/mixin)
  ())

(def render-xhtml :before primitive-inspector
  (when (edited? -self-)
    (ensure-client-state-sink -self-)))

;;;;;;
;;; T inspector

(def (component e) t-inspector (t/abstract primitive-inspector)
  ())

(def render-xhtml t-inspector
  (if (edited? -self-)
      (render-t-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Boolean inspector

(def (component e) boolean-inspector (boolean/abstract primitive-inspector)
  ())

(def render-xhtml boolean-inspector
  (bind (((:read-only-slots the-type edited client-state-sink) -self-)
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    <div ,(when edited
                <input (:type "hidden" :name ,(id-of client-state-sink) :value ,(if component-value "true" "false"))>)
         ,(if (eq the-type 'boolean)
              (bind ((checked (when component-value "checked"))
                     (disabled (unless edited "disabled")))
                <input (:type "checkbox" :checked ,checked :disabled ,disabled)>)
              (bind ((disabled (unless edited "disabled")))
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
;;; String inspector

(def (component e) string-inspector (string/abstract primitive-inspector)
  ())

(def render-xhtml string-inspector
  (bind (((:read-only-slots edited) -self-))
    (if edited
        (render-string-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; Password inspector

(def (component e) password-inspector (password/abstract string-inspector)
  ())

(def method print-component-value ((component password-inspector))
  (if (edited? component)
      (call-next-method)
      "**********"))

;;;;;;
;;; Symbol inspector

(def (component e) symbol-inspector (symbol/abstract string-inspector)
  ())

;;;;;;
;;; Number inspector

(def (component e) number-inspector (number/abstract primitive-inspector)
  ())

(def render-xhtml number-inspector
  (bind (((:read-only-slots edited) -self-))
    (if edited
        (render-number-field-for-primitive-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; Integer inspector

(def (component e) integer-inspector (integer/abstract number-inspector)
  ())

;;;;;;
;;; Float inspector

(def (component e) float-inspector (float/abstract number-inspector)
  ())

;;;;;;
;;; Date inspector

(def (component e) date-inspector (date/abstract primitive-inspector)
  ())

(def render-xhtml date-inspector
  (if (edited? -self-)
      (render-date-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Time inspector

(def (component e) time-inspector (time/abstract primitive-inspector)
  ())

(def render-xhtml time-inspector
  (if (edited? -self-)
      (render-time-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Timestamp inspector

(def (component e) timestamp-inspector (timestamp/abstract primitive-inspector)
  ())

(def render-xhtml timestamp-inspector
  (if (edited? -self-)
      (render-timestamp-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Member inspector

(def (component e) member-inspector (member/abstract primitive-inspector)
  ())

(def render-xhtml member-inspector
  (if (edited? -self-)
      (render-member-component -self-)
      (bind ((icon (find-member-component-value-icon -self-)))
        (if icon
            (render-icon :icon icon :label (print-component-value -self-) :tooltip nil)
            `xml,(print-component-value -self-)))))

;;;;;;
;;; HTML inspector

(def (component e) html-inspector (html/abstract primitive-inspector)
  ())

(def render-xhtml html-inspector
  (if (edited? -self-)
      (render-html-component -self-)
      (emit-html-component-value -self-)))

;;;;;;
;;; IP address inspector

(def (component e) ip-address-inspector (ip-address/abstract primitive-inspector)
  ())

(def method print-component-value ((self ip-address-inspector))
  (if (edited? self)
      (call-next-method)
      (bind ((value (component-value-of self)))
        (iolib:address-to-string
         (etypecase value
           (iolib:inet-address                    value)
           ((simple-array (unsigned-byte 8) (4))  (make-instance 'iolib:ipv4-address :name value))
           ((simple-array (unsigned-byte 16) (8)) (make-instance 'iolib:ipv6-address :name value)))))))

(def render-xhtml ip-address-inspector
  (if (edited? -self-)
      (call-next-method)
      <span (:class "ip-address")
        ,(print-component-value -self-)>))

;;;;;;
;;; File inspector

(def (component e) file-inspector (file/abstract primitive-inspector)
  ((upload-command :type component)
   (download-command :type component)
   (directory "/tmp/")
   (file-name)
   (url-prefix "static/")))

(def refresh-component file-inspector
  (bind (((:slots upload-command download-command directory file-name url-prefix) -self-)
         ((:values class instance slot) (extract-primitive-component-place -self-)))
    (setf upload-command (command ()
                           (icon upload)
                           (make-action
                             (execute-upload-file -self-))))
    (setf download-command (command ()
                             (icon download)
                             (make-action
                               (execute-download-file -self-))
                             :delayed-content #t
                             :path (download-file-name -self- class instance slot)))))

(def render-xhtml file-inspector
  (if (edited? -self-)
      (render-component (upload-command-of -self-))
      (render-component (download-command-of -self-))))

(def (layered-function e) execute-upload-file (component))

(def (layered-function e) execute-download-file (component))
