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
  ((name nil)
   (the-type nil)
   (component-value)
   (client-state-sink nil)))

(def render-csv primitive-component ()
  (render-csv-value (print-component-value -self-)))

(def function ensure-client-state-sink (component)
  (setf (client-state-sink-of component)
        (client-state-sink (client-value)
          (handler-bind ((invalid-client-value (lambda (error)
                                                 (setf (component-value-of component) error)
                                                 (return))))
            (bind (((:values value no-value?)
                    (parse-component-value component client-value)))
              (if no-value?
                  (slot-makunbound component 'component-value)
                  (setf (component-value-of component) value)))))))

(def method component-value-of :around ((-self- primitive-component))
  (bind ((result (call-next-method)))
    (if (typep result 'error)
        (error result)
        result)))

(def function component-value-and-bound-p (component)
  (bind ((has-component-value? (slot-boundp component 'component-value)))
    (values (when has-component-value?
              (component-value-of component))
            has-component-value?)))

(def generic parse-component-value (component client-value))

(def generic print-component-value (component))

(def resources en
  (value.default "default")
  (value.defaults-to "defaults to :"))

(def resources hu
  (value.default "alapértelmezett")
  (value.defaults-to "alapértelmezett érték: "))

;;;;;;
;;; Unbound component

(def component unbound-component (primitive-component)
  ())

(def method print-component-value ((self unbound-component))
  #"value.unbound")

(def (function io) render-unbound-component ()
  `xml,#"value.unbound")

(def render unbound-component ()
  (render-unbound-component))

(def resources en
  (value.unbound "default"))

(def resources hu
  (value.unbound "alapértelmezett"))

;;;;;;
;;; Null component

(def component null-component (primitive-component)
  ())

(def method print-component-value ((self null-component))
  #"value.nil")

(def (function io) render-null-component ()
  `xml,#"value.nil")

(def render null-component ()
  (render-null-component))

(def resources en
  (value.nil "none"))

(def resources hu
  (value.nil "nincs"))

;;;;;;
;;; T component

(def component t-component (primitive-component)
  ())

(def function render-t-component (component)
  (render-string-field "text" (print-component-value component) (client-state-sink-of component)))

(def method print-component-value ((component t-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (format nil "~S" component-value)
        #"value.unbound")))

(def method parse-component-value ((component t-component) client-value)
  ;; TODO: this is kind of dangerous
  (read-from-string client-value))

;;;;;;
;;; Boolean component

(def component boolean-component (primitive-component)
  ())

(def method parse-component-value ((component boolean-component) client-value)
  (if (string= client-value "")
      (values nil #t)
      (string-to-lisp-boolean client-value)))

(def resources en
  (boolean.true "true")
  (boolean.false "false"))

(def resources hu
  (boolean.true "igaz")
  (boolean.false "hamis"))

;;;;;;
;;; String component

(def component string-component (primitive-component)
  ())

(def generic string-field-type (component)
  (:method ((self string-component))
    "text"))

(def function render-string-component (component &key on-change)
  (render-string-field (string-field-type component)
                       (print-component-value component)
                       (client-state-sink-of component)
                       :on-change on-change))

(def method print-component-value ((component string-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        component-value)))

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

;;;;;;
;;; Symbol component

(def component symbol-component (string-component)
  ())

(def method print-component-value ((component symbol-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (symbol-name component-value))))

;;;;;;
;;; Number component

(def component number-component (primitive-component)
  ())

(def function render-number-component (component &key on-change)
  (render-number-field (print-component-value component)
                       (client-state-sink-of component)
                       :on-change on-change))

(def method print-component-value ((component number-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (princ-to-string component-value))))

(def method parse-component-value ((component number-component) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
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
      (values (parse-integer client-value))))

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

(def function render-date-component (component &key on-change (printer #'print-component-value))
  (bind (((:read-only-slots client-state-sink) component)
         (id (generate-frame-unique-string)))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,(funcall printer component)
              :dojoType #.+dijit/date-text-box+
              :onChange ,(force on-change))>)))

(def function print-date-value (value)
  (local-time:format-rfc3339-timestring nil value :omit-time-part #t :omit-timezone-part #t))

(def method print-component-value ((component date-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (print-date-value component-value)
        "")))

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

(def function render-time-component (component &key on-change (printer #'print-component-value))
  (bind (((:read-only-slots client-state-sink) component)
         (id (generate-frame-unique-string)))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :constraints "{timePattern:'HH:mm:ss', clickableIncrement:'T01:00:00', visibleIncrement:'T04:00:00', visibleRange:'T12:00:00'}"
              :value    ,(funcall printer component)
              :dojoType #.+dijit/time-text-box+
              :onChange ,(force on-change))>)))

(def function print-time-value (value)
  (local-time:format-timestring nil value :format '(#\T (:hour 2) #\: (:min 2) #\: (:sec 2)) :timezone local-time:+utc-zone+))

(def method print-component-value ((component time-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (print-time-value component-value)
        "")))

(def method parse-component-value ((component time-component) client-value)
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :allow-missing-date-part #t :allow-missing-timezone-part #t)
      (unless it
        (invalid-client-value "Failed to parse ~S as a time" client-value)))))

;;;;;;
;;; Timestamp component

(def component timestamp-component (primitive-component)
  ())

(def function render-timestamp-component (component &key on-change)
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (render-date-component component :on-change on-change :printer (lambda (component)
                                                                     (declare (ignore component))
                                                                     (if (and has-component-value?
                                                                              component-value)
                                                                         (print-date-value component-value)
                                                                         "")))
    (render-time-component component :on-change on-change :printer (lambda (component)
                                                                     (declare (ignore component))
                                                                     (if (and has-component-value?
                                                                              component-value)
                                                                         (print-time-value component-value)
                                                                         "")))))

(def method print-component-value ((component timestamp-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (localized-timestamp component-value)
        "")))

(def method parse-component-value ((component timestamp-component) client-value)
  (when (consp client-value)
    (setf client-value (apply #'concatenate-string client-value)))
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :fail-on-error #f)
      ;; TODO: timezone is not present in the string and thus this parsing fails: (local-time:parse-rfc3339-timestring client-value :fail-on-error #f)
      (unless it
        (invalid-client-value "Failed to parse ~S as a timestamp" client-value)))))

;;;;;;
;;; Member component

(def component member-component (primitive-component)
  ((possible-values)
   (comparator #'equal)
   (key #'identity)
   (client-name-generator 'localized-member-component)))

(def function localized-member-component (component value)
  (bind ((slot-value (find-ancestor-component-with-type component 'abstract-standard-object-slot-value-component)))
    (localized-member-component-value (when slot-value
                                        (the-class-of slot-value))
                                      (when slot-value
                                        (slot-of slot-value))
                                      value)))

(def generic localized-member-component-value (class slot value)
  (:method (class slot value)
    (localized-enumeration-member value :class class :slot slot :capitalize-first-letter #t)))

(def function member-component-value-name (value)
  (typecase value
    (symbol (symbol-name value))
    (class (class-name value))
    (t (write-to-string value))))

;; TODO: KLUDGE: we must use a string key here because we don't know the package
(def function find-member-component-value-icon (component)
  (when (slot-boundp component 'component-value)
    (bind ((slot-value (find-ancestor-component-with-type component 'abstract-standard-object-slot-value-component)))
      (when slot-value
        (bind ((slot-name (slot-definition-name (slot-of slot-value))))
          (find-icon (format-symbol (symbol-package slot-name)
                                    "~A-~A"
                                    slot-name
                                    (member-component-value-name (component-value-of component)))
                     :otherwise nil))))))

(def method print-component-value ((component member-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (funcall (client-name-generator-of component) component component-value)
        "")))

(def method parse-component-value ((component member-component) client-value)
  (bind (((:read-only-slots possible-values) component)
         (index (parse-integer client-value)))
    (assert (< index (length possible-values)))
    (elt possible-values index)))

(def function render-member-component (component &key on-change)
  (bind (((:read-only-slots possible-values client-name-generator client-state-sink) component)
         (has-component-value? (slot-boundp component 'component-value))
         (component-value (when has-component-value?
                            (component-value-of component))))
    (render-select-field component-value possible-values :name (id-of client-state-sink)
                         :client-name-generator [funcall client-name-generator component !1]
                         :on-change on-change)))

(def resources en
  (member-type-value.nil ""))

(def resources hu
  (member-type-value.nil ""))


;;;;;;
;;; HTML component

(def component html-component (string-component)
  ())

(def function emit-html-component-value (component)
  (write-sequence (babel:string-to-octets (print-component-value component) :encoding :utf-8) *html-stream*)
  (values))

(def function render-html-component (component)
  (bind ((id (generate-frame-unique-string))
         (field-id (generate-frame-unique-string)))
    (render-dojo-widget (id)
      <input (:id ,field-id
              :name ,(id-of (client-state-sink-of component))
              :value ,(print-component-value component)
              :type "hidden")>
      <div (:id       ,id
            :dojoType #.+dijit/editor+
            :extraPlugins "['foreColor','hiliteColor',{name:'dijit._editor.plugins.FontChoice', command:'fontName', generic:true},'fontSize','createLink','insertImage']"
            :onChange `js-inline(setf (slot-value (dojo.byId ,field-id) 'value) (.getValue (dijit.byId ,id))))
        ,(emit-html-component-value component)>)))

;;;;;;
;;; IP address component

(def component ip-address-component (primitive-component)
  ())

(def render ip-address-component ()
  `xml,(print-component-value -self-))

(def method print-component-value ((component ip-address-component))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (with-output-to-string (string)
          (iter (for ip-element :in-sequence component-value)
                (unless (first-iteration-p)
                  (write-char #\. string))
                (write-string (princ-to-string ip-element) string)))
        "")))
