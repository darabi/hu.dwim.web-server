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

;;;;;;
;;; T maker

(def component t-maker (t-component primitive-maker)
  ())

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
    <span ,(if (or (and (eq the-type 'boolean)
                        has-initform?
                        constant-initform?)
                   (and has-initform?
                        constant-initform?))
               <input (:type "checkbox"
                             ,(when (and has-initform?
                                         (eq initform #t))
                                    (make-xml-attribute "checked" "checked")))>
               <select ()
                 ;; TODO: add error marker when no initform and default value is selected
                 <option ,(cond (has-initform? #"value.default")
                                ((eq the-type 'boolean) "")
                                (t #"value.nil"))>
                 <option ,#"boolean.true">
                 <option ,#"boolean.false">>)
          ,(when has-initform?
                 <span ,#"value.defaults-to" ,(princ-to-string initform)>)>))

;;;;;;
;;; String maker

(def component string-maker (string-component primitive-maker)
  ((component-value nil)))

(def render string-maker ()
  (render-string-field -self-))

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
