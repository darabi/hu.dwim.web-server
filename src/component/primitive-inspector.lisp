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
  (bind (((:read-only-slots edited component-value) -self-)
         (printed-value (format nil "~S" component-value)))
    (if edited
        (render-t-component -self-)
        `xml,printed-value)))

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
              <input (:type "checkbox"
                            ,(when component-value
                                   (make-xml-attribute "checked" "checked"))
                            ,(unless edited
                                     (make-xml-attribute "disabled" "disabled")))>
              <select (,(unless edited
                                (make-xml-attribute "disabled" "disabled")))
                ;; TODO: add error marker when no initform and default value is selected
                <option (,(unless has-component-value?
                                  (make-xml-attribute "selected" "yes")))
                        ,#"value.nil">
                <option (,(when (and has-component-value?
                                     component-value)
                                (make-xml-attribute "selected" "yes")))
                        ,#"boolean.true">
                <option (,(when (and has-component-value?
                                     (not component-value))
                                (make-xml-attribute "selected" "yes")))
                        ,#"boolean.false">>)>))

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

;;;;;;
;;; Time inspector

(def component time-inspector (time-component primitive-inspector)
  ())

;;;;;;
;;; Timestamp inspector

(def component timestamp-inspector (timestamp-component primitive-inspector)
  ())

;;;;;;
;;; Member inspector

(def component member-inspector (member-component primitive-inspector)
  ())

(def render member-inspector ()
  (if (edited-p -self-)
      (render-member-component -self-)
      `xml,(print-component-value -self-)))
