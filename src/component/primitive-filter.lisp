;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ((enabled #f :type boolean)))

(def render :before primitive-filter
  #+nil
  (ensure-client-state-sink -self-))

(def layered-function render-filter-predicate (component)
  (:method ((self component))
    <td>
    <td>))

(def function render-enabled-marker (component)
  <input (:type "checkbox"
          ,(unless (enabled-p component)
             (make-xml-attribute "disabled" "disabled"))
          ,(when (enabled-p component)
             (make-xml-attribute "checked" "checked")))>)

;;;;;;
;;; T filter

(def component t-filter (t-component primitive-filter)
  ())

(def render t-filter ()
  (render-t-component -self-))

;;;;;;
;;; Boolean filter

(def component boolean-filter (boolean-component primitive-filter)
  ())

(def render boolean-filter ()
  (setf (client-state-sink-of -self-)
        (client-state-sink (client-value)
          (bind ((index (parse-integer client-value)))
            (setf (enabled-p -self-) (not (zerop index)))
            (ecase index
              (0)
              (1 (slot-makunbound -self- 'component-value))
              (2 (setf (component-value-of -self-) #t))
              (3 (setf (component-value-of -self-) #f))))))
  (render-enabled-marker -self-)
  (bind ((enabled (enabled-p -self-))
         (has-component-value? (slot-boundp -self- 'component-value))
         (component-value (when has-component-value?
                            (component-value-of -self-))))
    <select (:name ,(id-of (client-state-sink-of -self-)))
      <option (:value 0
               ,(unless enabled
                  (make-xml-attribute "selected" "yes")))>
      ,(unless (eq (the-type-of -self-) 'boolean)
               <option (:value 1
                        ,(when (and enabled
                                    (not has-component-value?))
                           (make-xml-attribute "selected" "yes")))
                       ,#"value.nil">)
      <option (:value 2
               ,(when (and enabled
                           has-component-value?
                           component-value)
                  (make-xml-attribute "selected" "yes")))
              ,#"boolean.true">
      <option (:value 3
               ,(when (and enabled
                           has-component-value?
                           (not component-value))
                  (make-xml-attribute "selected" "yes")))
              ,#"boolean.false">>))

;;;;;;
;;; String filter

(def component string-filter (string-component primitive-filter)
  ((component-value nil)))

(def render string-filter ()
  (ensure-client-state-sink -self-)
  (render-enabled-marker -self-)
  (render-string-component -self-))

(def method parse-component-value :before ((component string-filter) client-value)
  (setf (enabled-p component)
        (or (not (string= "" client-value))
            (null-subtype-p (the-type-of component)))))

;;;;;;
;;; Password filter

(def component password-filter (password-component string-filter)
  ())

;;;;;;
;;; Symbol filter

(def component symbol-filter (symbol-component string-filter)
  ())

;;;;;;
;;; Number filter

(def component number-filter (number-component primitive-filter)
  ())

(def render number-filter ()
  (if (null-subtype-p (the-type-of -self-))
      <span <input (:type "checkbox")>
            ,(render-number-component -self-)>
      (render-number-component -self-)))

;;;;;;
;;; Integer filter

(def component integer-filter (integer-component number-filter)
  ())

;;;;;;
;;; Float filter

(def component float-filter (float-component number-filter)
  ())

;;;;;;
;;; Date filter

(def component date-filter (date-component primitive-filter)
  ())

;;;;;;
;;; Time filter

(def component time-filter (time-component primitive-filter)
  ())

;;;;;;
;;; Timestamp filter

(def component timestamp-filter (timestamp-component primitive-filter)
  ())

;;;;;;
;;; Member filter

(def component member-filter (member-component primitive-filter)
  ())











;;;;;;
;;; TODO:
#|
;; TODO: all predicates
(def (constant :test 'equalp) +filter-predicates+ '(equal like < <= > >= #+nil(between)))

          label (label (localized-slot-name slot))
          negate-command (make-instance 'command-component
                                        :icon (make-negated/ponated-icon negated)
                                        :action (make-action
                                                  (setf negated (not negated))
                                                  (setf (icon-of negate-command) (make-negated/ponated-icon negated))))
          predicate-command (make-instance 'command-component
                                           :icon (make-predicate-icon predicate)
                                           :action (make-action
                                                     (setf predicate (elt +filter-predicates+
                                                                          (mod (1+ (position predicate +filter-predicates+))
                                                                               (length +filter-predicates+))))
                                                     (setf (icon-of predicate-command) (make-predicate-icon predicate))))

(def function make-negated/ponated-icon (negated)
  (aprog1 (make-icon-component (if negated 'negated 'ponated))
    (setf (label-of it) nil)))

(def function make-predicate-icon (predicate)
  (aprog1 (make-icon-component predicate)
    (setf (label-of it) nil)))
|#