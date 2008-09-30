;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Invalid client value

(def condition* invalid-client-value (simple-error)
  ())

(def function invalid-client-value (message &rest args)
  (error 'invalid-client-value :format-control message :format-arguments args))

#+nil
(def macro with-client-value-error-wrapper (&body body)
  `(handler-bind ((error (lambda (error)
                           (invalid-client-value "Failed to parse client value because: ~A" error))))
     ,@body))

;;;;;;
;;; Primitive component

(def component primitive-component ()
  ((the-type)
   (component-value)
   (client-state-sink nil)))

(def function ensure-client-state-sink (component)
  (setf (client-state-sink-of component)
        (client-state-sink (client-value)
          (handler-bind ((invalid-client-value (lambda (error)
                                                 (setf (component-value-of component) error)
                                                 (return))))
            (setf (component-value-of component) (parse-component-value component client-value))))))

(def method component-value-of :around ((-self- primitive-component))
  (bind ((result (call-next-method)))
    (if (typep result 'error)
        (error result)
        result)))

(def generic parse-component-value (component client-value))

(def generic print-component-value (component component-value))

(defresources en
  (value.default "default")
  (value.defaults-to "defaults to :"))

(defresources hu
  (value.default "alapértelmezett")
  (value.defaults-to "alapértelmezett érték: "))

;;;;;;
;;; Unbound component

(def component unbound-component (primitive-component)
  ())

(def render unbound-component ()
  <span ,#"value.unbound">)

(defresources en
  (value.unbound "default"))

(defresources hu
  (value.unbound "alapértelmezett"))

;;;;;;
;;; Null component

(def component null-component (primitive-component)
  ())

(def render null-component ()
  <span ,#"value.nil">)

(defresources en
  (value.nil "none"))

(defresources hu
  (value.nil "nincs"))

;;;;;;
;;; T component

(def component t-component (primitive-component)
  ())

;;;;;;
;;; Boolean component

(def component boolean-component (primitive-component)
  ())

(def method parse-component-value ((component boolean-component) client-value)
  (cond ((string= "#t" client-value)
         #t)
        ((string= "#f" client-value)
         #f)
        (t (error "Unknown boolean value ~A" client-value))))

(defresources en
  (boolean.true "true")
  (boolean.false "false"))

(defresources hu
  (boolean.true "igaz")
  (boolean.false "hamis"))

;;;;;;
;;; String component

(def component string-component (primitive-component)
  ())

(def generic string-field-type (component)
  (:method ((self string-component))
    "text"))

(def function render-string-field (component)
  (bind ((id (generate-frame-unique-string "_w")))
    (render-dojo-widget (id)
      ;; TODO dojoRows 3
      <input (:type     ,(string-field-type component)
              :id       ,id
              :name     ,(id-of (client-state-sink-of component))
              :value    ,(print-component-value component (component-value-of component))
              :dojoType #.+dijit/text-box+)>)))

(def method print-component-value ((component string-component) component-value)
  (if (null component-value)
      ""
      component-value))

(def method parse-component-value ((component string-component) client-value)
  (if (string= client-value "")
      nil
      client-value))

;;;;;;
;;; Password component

(def component password-component (string-component)
  ())

(def method string-field-type ((self password-component))
  "password")

(def method print-component-value :around ((component password-component) component-value)
  (if (or (null component-value)
          (string= "" component-value))
      ""
      (call-next-method)))

(def method print-component-value ((component password-component) component-value)
  component-value)

;;;;;;
;;; Symbol component

(def component symbol-component (string-component)
  ())

;;;;;;
;;; Number component

(def component number-component (primitive-component)
  ())

(def method parse-component-value ((component number-component) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-number client-value)))

;;;;;;
;;; Integer component

(def component integer-component (number-component)
  ())

(def method parse-component-value ((component integer-component) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
      nil
      (parse-integer client-value)))

;;;;;;
;;; Float component

(def component float-component (number-component)
  ())

(def method parse-component-value ((component float-component) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-real-number client-value)))

;;;;;;
;;; Date component

(def component date-component (primitive-component)
  ())

(def method parse-component-value ((component date-component) client-value)
  (unless (string= client-value "")
    (bind ((result (local-time:parse-rfc3339-timestring client-value :allow-missing-time-part #t)))
      (local-time:with-decoded-timestamp (:hour hour :minute minute :sec sec :nsec nsec :timezone local-time:+utc-zone+) result
        (unless (and (zerop hour)
                     (zerop minute)
                     (zerop sec)
                     (zerop nsec))
          (invalid-client-value "~S is not a valid date" result)))
      result)))

;;;;;;
;;; Time component

(def component time-component (primitive-component)
  ())

;;;;;;
;;; Timestamp component

(def component timestamp-component (primitive-component)
  ())

(def method parse-component-value ((component timestamp-component) client-value)
  (unless (string= client-value "")
    (aprog1
        (local-time:parse-rfc3339-timestring client-value :fail-on-error #f)
      (unless it
        (invalid-client-value "Failed to parse ~S as a timestamp" client-value)))))

;;;;;;
;;; Member component

(def component member-component (primitive-component)
  ((possible-values)
   (comparator #'equal)
   (key #'identity)
   ;; TODO we need to get the class and slot here somehow
   (client-name-generator [localized-member-component-value nil nil !1])))

(def generic localized-member-component-value (class slot value)
  (:method (class slot value)
    (if value
        (localized-enumeration-member value :class class :slot slot :capitalize-first-letter #t)
        "")))

(def method parse-component-value ((component member-component) client-value)
  (bind (((:read-only-slots possible-values allow-nil-value) component)
         (index (parse-integer client-value)))
    (if (and allow-nil-value
             (= index 0))
        nil
        (progn
          (when allow-nil-value
            (decf index))
          (assert (< index (length possible-values)))
          (elt possible-values index)))))
