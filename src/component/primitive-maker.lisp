;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive maker

(def component primitive-maker (primitive-component maker-component)
  ((initform)))

(def render :before primitive-maker
  (ensure-client-state-sink -self-))

(def function render-initform (component)
  (when (slot-boundp component 'initform)
    <span ,#"value.defaults-to" ,(princ-to-string (initform-of component))>))

;;;;;;
;;; T maker

(def component t-maker (t-component primitive-maker)
  ())

(def render t-maker ()
  (render-t-component -self-))

;;;;;;
;;; Boolean maker

(def component boolean-maker (boolean-component primitive-maker)
  ())

(def render boolean-maker ()
  (bind (((:read-only-slots the-type) -self-)
         (has-initform? (slot-boundp -self- 'initform))
         (initform (when has-initform?
                     (initform-of -self-)))
         (constant-initform? (member initform '(#f #t))))
    <span ,(if (and (eq the-type 'boolean)
                    has-initform?
                    constant-initform?)
               <input (:type "checkbox"
                             ,(when (and has-initform?
                                         (eq initform #t))
                                    (make-xml-attribute "checked" "checked")))>
               <select ()
                 ;; TODO: add error marker when no initform and default value is selected
                 <option (,(unless (and has-initform?
                                        constant-initform?)
                                   (make-xml-attribute "selected" "yes")))
                         ,(cond (has-initform? #"value.default")
                                ((eq the-type 'boolean) "")
                                (t #"value.nil"))>
                 <option (,(when (and has-initform?
                                      (eq initform #t))
                                 (make-xml-attribute "selected" "yes")))
                         ,#"boolean.true">
                 <option (,(when (and has-initform?
                                      (eq initform #f))
                                 (make-xml-attribute "selected" "yes")))
                         ,#"boolean.false">>)
          ,(render-initform -self-)>))

;;;;;;
;;; String maker

(def component string-maker (string-component primitive-maker)
  ((component-value nil)))

(def render string-maker ()
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

(def render number-maker ()
  <span ,(render-number-component -self-)
        ,(render-initform -self-)>)

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

;;;;;;
;;; Time maker

(def component time-maker (time-component primitive-maker)
  ())

;;;;;;
;;; Timestamp maker

(def component timestamp-maker (timestamp-component primitive-maker)
  ())

;;;;;;
;;; Member maker

(def component member-maker (member-component primitive-maker)
  ())
