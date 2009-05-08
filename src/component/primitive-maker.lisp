;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive maker

(def component primitive-maker (primitive-component maker-component)
  ((initform)
   (use-initform :type boolean)))

(def constructor primitive-maker ()
  (setf (use-initform-p -self-)
        (slot-boundp -self- 'initform)))

(def render-xhtml :before primitive-maker
  (ensure-client-state-sink -self-))

(def function render-initform (component)
  (when (slot-boundp component 'initform)
    <span ,#"value.defaults-to" ,(princ-to-string (initform-of component))>))

;;;;;;
;;; T maker

(def component t-maker (t-component primitive-maker)
  ())

(def render-xhtml t-maker
  (render-t-component -self-))

;;;;;;
;;; Boolean maker

(def component boolean-maker (boolean-component primitive-maker)
  ())

(def render-xhtml boolean-maker
  (bind (((:read-only-slots the-type) -self-)
         (has-initform? (slot-boundp -self- 'initform))
         (initform (when has-initform?
                     (initform-of -self-)))
         (constant-initform? (member initform '(#f #t))))
    (if (and (eq the-type 'boolean)
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
                                ((eq the-type 'boolean) "")
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
;;; String maker

(def component string-maker (string-component primitive-maker)
  ((component-value nil)))

(def render-xhtml string-maker
  (render-string-component -self-))

;;;;;;
;;; Password maker

(def component password-maker (password-component string-maker)
  ())

;;;;;;
;;; Symbol maker

(def component symbol-maker (symbol-component string-maker)
  ())

;;;;;;
;;; Number maker

(def component number-maker (number-component primitive-maker)
  ())

(def render-xhtml number-maker
  (render-number-field-for-primitive-component -self-))

;;;;;;
;;; Integer maker

(def component integer-maker (integer-component number-maker)
  ())

;;;;;;
;;; Float maker

(def component float-maker (float-component number-maker)
  ())

;;;;;;
;;; Date maker

(def component date-maker (date-component primitive-maker)
  ())

(def render-xhtml date-maker
  (render-date-component -self-))

;;;;;;
;;; Time maker

(def component time-maker (time-component primitive-maker)
  ())

(def render-xhtml time-maker
  (render-time-component -self-))

;;;;;;
;;; Timestamp maker

(def component timestamp-maker (timestamp-component primitive-maker)
  ())

(def render-xhtml timestamp-maker
  (render-timestamp-component -self-))

;;;;;;
;;; Member maker

(def component member-maker (member-component primitive-maker)
  ())

(def render-xhtml member-maker
  (render-member-component -self-))

;;;;;;
;;; HTML inspector

(def component html-maker (html-component primitive-maker)
  ())

(def render-xhtml html-maker
  (render-html-component -self-))

;;;;;;
;;; IP address maker

(def component ip-address-maker (ip-address-component primitive-maker)
  ())
