;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive inspector

(def component primitive-inspector (primitive-component inspector-component editable-component)
  ())

(def render :before primitive-inspector
  (when (edited-p -self-)
    (ensure-client-state-sink -self-)))

;;;;;;
;;; T inspector

(def component t-inspector (t-component primitive-inspector)
  ())

(def render t-inspector ()
  (if (edited-p -self-)
      (render-t-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Boolean inspector

(def component boolean-inspector (boolean-component primitive-inspector)
  ())

(def render boolean-inspector ()
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

(def component string-inspector (string-component primitive-inspector)
  ())

(def render string-inspector ()
  (bind (((:read-only-slots edited) -self-))
    (if edited
        (render-string-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; Password inspector

(def component password-inspector (password-component string-inspector)
  ())

(def method print-component-value ((component password-inspector))
  (if (edited-p component)
      (call-next-method)
      "**********"))

;;;;;;
;;; Symbol inspector

(def component symbol-inspector (symbol-component string-inspector)
  ())

;;;;;;
;;; Number inspector

(def component number-inspector (number-component primitive-inspector)
  ())

(def render number-inspector ()
  (bind (((:read-only-slots edited) -self-))
    (if edited
        (render-number-component -self-)
        `xml,(print-component-value -self-))))

;;;;;;
;;; Integer inspector

(def component integer-inspector (integer-component number-inspector)
  ())

;;;;;;
;;; Float inspector

(def component float-inspector (float-component number-inspector)
  ())

;;;;;;
;;; Date inspector

(def component date-inspector (date-component primitive-inspector)
  ())

(def render date-inspector ()
  (if (edited-p -self-)
      (render-date-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Time inspector

(def component time-inspector (time-component primitive-inspector)
  ())

(def render time-inspector ()
  (if (edited-p -self-)
      (render-time-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Timestamp inspector

(def component timestamp-inspector (timestamp-component primitive-inspector)
  ())

(def render timestamp-inspector ()
  (if (edited-p -self-)
      (render-timestamp-component -self-)
      `xml,(print-component-value -self-)))

;;;;;;
;;; Member inspector

(def component member-inspector (member-component primitive-inspector)
  ())

(def render member-inspector ()
  (if (edited-p -self-)
      (render-member-component -self-)
      (bind ((icon (find-member-component-value-icon -self-)))
        (when icon
          (render-icon icon :image-path (image-path-of icon) :name (name-of icon)))
        `xml,(print-component-value -self-))))

;;;;;;
;;; HTML inspector

(def component html-inspector (html-component primitive-inspector)
  ())

(def render html-inspector ()
  (if (edited-p -self-)
      (render-html-component -self-)
      (emit-html-component-value -self-)))

;;;;;;
;;; IP address inspector

(def component ip-address-inspector (ip-address-component primitive-inspector)
  ())
