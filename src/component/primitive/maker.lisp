;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive maker

(def (component ea) primitive/maker (primitive/abstract maker/abstract)
  ((initform)
   (use-initform :type boolean)))

(def constructor primitive/maker ()
  (setf (use-initform-p -self-)
        (slot-boundp -self- 'initform)))

(def render-xhtml :before primitive/maker
  (ensure-client-state-sink -self-))

(def function render-initform (component)
  (when (slot-boundp component 'initform)
    <span ,#"value.defaults-to" ,(princ-to-string (initform-of component))>))

;;;;;;
;;; T maker

(def (component ea) t/maker (t/abstract primitive/maker)
  ())

(def render-xhtml t/maker
  (render-t-component -self-))

;;;;;;
;;; Boolean maker

(def (component ea) boolean/maker (boolean/abstract primitive/maker)
  ())

(def render-xhtml boolean/maker
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

(def (component ea) string/maker (string/abstract primitive/maker)
  ((component-value nil)))

(def render-xhtml string/maker
  (render-string-component -self-))

;;;;;;
;;; Password maker

(def (component ea) password/maker (password/abstract string/maker)
  ())

;;;;;;
;;; Symbol maker

(def (component ea) symbol/maker (symbol/abstract string/maker)
  ())

;;;;;;
;;; Number maker

(def (component ea) number/maker (number/abstract primitive/maker)
  ())

(def render-xhtml number/maker
  (render-number-field-for-primitive-component -self-))

;;;;;;
;;; Integer maker

(def (component ea) integer/maker (integer/abstract number/maker)
  ())

;;;;;;
;;; Float maker

(def (component ea) float/maker (float/abstract number/maker)
  ())

;;;;;;
;;; Date maker

(def (component ea) date/maker (date/abstract primitive/maker)
  ())

(def render-xhtml date/maker
  (render-date-component -self-))

;;;;;;
;;; Time maker

(def (component ea) time/maker (time/abstract primitive/maker)
  ())

(def render-xhtml time/maker
  (render-time-component -self-))

;;;;;;
;;; Timestamp maker

(def (component ea) timestamp/maker (timestamp/abstract primitive/maker)
  ())

(def render-xhtml timestamp/maker
  (render-timestamp-component -self-))

;;;;;;
;;; Member maker

(def (component ea) member/maker (member/abstract primitive/maker)
  ())

(def render-xhtml member/maker
  (render-member-component -self-))

;;;;;;
;;; HTML inspector

(def (component ea) html/maker (html/abstract primitive/maker)
  ())

(def render-xhtml html/maker
  (render-html-component -self-))

;;;;;;
;;; IP address maker

(def (component ea) ip-address/maker (ip-address/abstract primitive/maker)
  ())
