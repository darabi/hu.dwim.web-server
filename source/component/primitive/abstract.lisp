;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Invalid client value

(def condition* invalid-client-value (simple-error)
  ())

(def function invalid-client-value (message &rest args)
  (error 'invalid-client-value :format-control message :format-arguments args))

;;;;;;
;;; Util

(def function ensure-client-state-sink (component)
  (setf (client-state-sink-of component)
        (client-state-sink (client-value)
          (handler-bind ((invalid-client-value (lambda (error)
                                                 (setf (component-value-of component) error)
                                                 (return))))
            (bind (((:values new-value no-value?) (parse-component-value component client-value))
                   (bound? (slot-boundp component 'component-value))
                   (old-value (when bound? (component-value-of component))))
              (if no-value?
                  (when bound?
                    (slot-makunbound component 'component-value))
                  (unless (and bound?
                               (equal old-value new-value))
                    (setf (component-value-of component) new-value))))))))

(def function component-value-and-bound-p (component)
  (bind ((has-component-value? (slot-boundp component 'component-value)))
    (values (when has-component-value?
              (component-value-of component))
            has-component-value?)))

;;;;;;
;;; unbound/abstract

(def (component e) unbound/abstract (primitive/abstract)
  ())

(def method print-component-value ((self unbound/abstract))
  #"value.unbound")

(def (function io) render-unbound-component ()
  `xml,#"value.unbound")

(def render-xhtml unbound/abstract
  (render-unbound-component))

;;;;;;
;;; null/abstract

(def (component e) null/abstract (primitive/abstract)
  ())

(def method print-component-value ((self null/abstract))
  #"value.nil")

(def (function io) render-null-component ()
  `xml,#"value.nil")

(def render-xhtml null/abstract
  (render-null-component))

;;;;;;
;;; t/abstract

(def (component e) t/abstract (primitive/abstract)
  ())

(def function render-t-component (component)
  (render-string-field "text" (print-component-value component) (client-state-sink-of component)))

(def method print-component-value ((component t/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (format nil "~S" component-value)
        #"value.unbound")))

(def method parse-component-value ((component t/abstract) client-value)
  (if (zerop (length client-value))
      (values nil #t)
      (bind ((*read-eval* #f))
        ;; TODO: READ-FROM-STRING is kind of dangerous, even with *READ-EVAL* #f
        (values (read-from-string client-value)))))

;;;;;;
;;; boolean/abstract

(def (component e) boolean/abstract (primitive/abstract)
  ())

(def method parse-component-value ((component boolean/abstract) client-value)
  (if (string= client-value "")
      (values nil #t)
      (string-to-lisp-boolean client-value)))

;;;;;;
;;; string/abstract

(def (component e) string/abstract (primitive/abstract)
  ())

(def generic string-field-type (component)
  (:method ((self string/abstract))
    "text"))

(def function render-string-component (component &key (id (generate-frame-unique-string "_stw")) on-change on-key-down on-key-up)
  (render-string-field (string-field-type component)
                       (print-component-value component)
                       (client-state-sink-of component)
                       :id id
                       :on-change on-change
                       :on-key-down on-key-down
                       :on-key-up on-key-up))

(def method print-component-value ((component string/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        component-value)))

(def method parse-component-value ((component string/abstract) client-value)
  (if (string= client-value "")
      nil
      client-value))

;;;;;;
;;; password/abstract

(def (component e) password/abstract (string/abstract)
  ())

(def method string-field-type ((self password/abstract))
  "password")

;;;;;;
;;; symbol/abstract

(def (component e) symbol/abstract (string/abstract)
  ())

(def method print-component-value ((component symbol/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (qualified-symbol-name component-value))))

;;;;;;
;;; number/abstract

(def (component e) number/abstract (primitive/abstract)
  ())

(def function render-number-field-for-primitive-component (component &key (id (generate-frame-unique-string "_stw")) on-change on-key-up on-key-down)
  ;; TODO was print-component-value, but spaces are not accepted as a value of the <input>
  (bind ((component-value (component-value-and-bound-p component)))
    (render-number-field (if (null component-value)
                             ""
                             (princ-to-string component-value))
                         (client-state-sink-of component)
                         :id id
                         :on-change on-change
                         :on-key-up on-key-up
                         :on-key-down on-key-down)))

(def method print-component-value ((component number/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (format-number/decimal nil component-value))))

(def method parse-component-value ((component number/abstract) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
      nil
      (parse-number:parse-number client-value)))

;;;;;;
;;; integer/abstract

(def (component e) integer/abstract (number/abstract)
  ())

(def render-csv integer/abstract
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p -self-)))
    (when (and has-component-value?
            (not (null component-value)))
      (write-csv-value (princ-to-string component-value)))))


(def method parse-component-value ((component integer/abstract) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
      nil
      (values (parse-integer client-value))))

;;;;;;
;;; float/abstract

(def (component e) float/abstract (number/abstract)
  ())

(def method parse-component-value ((component float/abstract) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-real-number client-value)))

;;;;;;
;;; date/abstract

(def (component e) date/abstract (primitive/abstract)
  ())

(def function render-date-component (component &key (id (generate-frame-unique-string "_dtw")) on-change (printer #'print-component-value))
  (bind (((:read-only-slots client-state-sink) component))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :value    ,(funcall printer component)
              :dojoType #.+dijit/date-text-box+
              :onChange ,(force on-change))>)))

(def function print-date-value (value)
  (local-time:format-rfc3339-timestring nil value :omit-time-part #t :omit-timezone-part #t))

(def method print-component-value ((component date/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (print-date-value component-value)
        "")))

(def method parse-component-value ((component date/abstract) client-value)
  (unless (string= client-value "")
    (bind ((result (local-time:parse-rfc3339-timestring client-value :allow-missing-time-part #t)))
      (local-time:with-decoded-timestamp (:hour hour :minute minute :sec sec :nsec nsec :timezone local-time:+utc-zone+) result
        (unless (and (zerop hour)
                     (zerop minute)
                     (zerop sec)
                     (zerop nsec))
          (invalid-client-value "Failed to parse ~S as a date" client-value)))
      result)))

;;;;;;
;;; time/abstract

(def (component e) time/abstract (primitive/abstract)
  ())

(def function render-time-component (component &key (id (generate-frame-unique-string "_tmw")) on-change (printer #'print-component-value))
  (bind (((:read-only-slots client-state-sink) component))
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

(def method print-component-value ((component time/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (print-time-value component-value)
        "")))

(def method parse-component-value ((component time/abstract) client-value)
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :allow-missing-date-part #t :allow-missing-timezone-part #t)
      (unless it
        (invalid-client-value "Failed to parse ~S as a time" client-value)))))

;;;;;;
;;; timestamp/abstract

(def (component e) timestamp/abstract (primitive/abstract)
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

(def method print-component-value ((component timestamp/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if (and has-component-value?
             component-value)
        (localized-timestamp component-value)
        "")))

(def method parse-component-value ((component timestamp/abstract) client-value)
  (when (consp client-value)
    (setf client-value (apply #'concatenate-string client-value)))
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :fail-on-error #f)
      ;; TODO: timezone is not present in the string and thus this parsing fails: (local-time:parse-rfc3339-timestring client-value :fail-on-error #f)
      (unless it
        (invalid-client-value "Failed to parse ~S as a timestamp" client-value)))))

;;;;;;
;;; member/abstract

(def (component e) member/abstract (primitive/abstract)
  ((possible-values)
   (comparator #'equal)
   (key #'identity)
   (client-name-generator 'localized-member-component)))

(def function localized-member-component (component value)
  (bind (((:values class nil slot) (extract-primitive-component-place component)))
    (localized-member-component-value class slot value)))

(def generic localized-member-component-value (class slot value)
  (:method (class slot value)
    (localized-enumeration-member value :class class :slot slot :capitalize-first-letter #t)))

(def function find-member-component-value-icon (component)
  (when (slot-boundp component 'component-value)
    (bind (((:values nil nil slot) (extract-primitive-component-place component)))
      (when slot
        (bind ((slot-name (slot-definition-name slot)))
          (find-icon (format-symbol (symbol-package slot-name)
                                    "~A-~A"
                                    slot-name
                                    (member-component-value-name (component-value-of component)))
                     :otherwise nil))))))

(def method print-component-value ((component member/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (funcall (client-name-generator-of component) component component-value)
        "")))

(def method parse-component-value ((component member/abstract) client-value)
  (bind (((:read-only-slots possible-values) component)
         (index (ignore-errors (parse-integer client-value))))
    (unless index
      (invalid-client-value "Failed to parse ~S as a member index" client-value))
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

;;;;;;
;;; html/abstract

(def (component e) html/abstract (string/abstract)
  ())

(def (function e) emit-html-string (string)
  (write-sequence (babel:string-to-octets string :encoding +default-encoding+) *xml-stream*)
  (values))

(def (function e) emit-html-component-value (component)
  (emit-html-string (print-component-value component)))

(def function render-html-component (component)
  (bind ((id (generate-frame-unique-string))
         (field-id (generate-frame-unique-string)))
    (render-dojo-widget (id)
      <input (:id ,field-id
              :name ,(id-of (client-state-sink-of component))
              :value ,(print-component-value component)
              :type "hidden")>
      <div (:id ,id
            :dojoType #.+dijit/editor+
            :extraPlugins "['dijit._editor.plugins.AlwaysShowToolbar','foreColor','hiliteColor',{name:'dijit._editor.plugins.FontChoice', command:'fontName', generic:true},'fontSize','createLink','insertImage']"
            ;; TODO: according to the documentation the :height should be "", so that it will be adapted to content automatically
            :height "75px" :minHeight "75px" :maxHeight "200px"
            :onChange `js-inline(setf (slot-value (dojo.byId ,field-id) 'value) (.getValue (dijit.byId ,id))))
        ,(emit-html-component-value component)>)))

;;;;;;
;;; ip-address/abstract

(def (component e) ip-address/abstract (primitive/abstract)
  ())

(def render-xhtml ip-address/abstract
  `xml,(print-component-value -self-))

(def method print-component-value ((component ip-address/abstract))
  (bind (((:values component-value has-component-value?) (component-value-and-bound-p component)))
    (if has-component-value?
        (with-output-to-string (string)
          (iter (for ip-element :in-sequence component-value)
                (unless (first-iteration-p)
                  (write-char #\. string))
                (write-string (princ-to-string ip-element) string)))
        "")))

;;;;;;
;;; file/abstract

(def (component e) file/abstract (primitive/abstract)
  ())

(def (layered-function e) download-file-name (component class instance slot)
  (:method ((component file/abstract) class instance slot)
    ;; TODO wtf? fixme or delme
    (random-string)))
