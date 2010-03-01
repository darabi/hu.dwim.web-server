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

(def function component-value-and-bound? (component)
  (bind ((has-component-value? (slot-boundp component 'component-value)))
    (values (when has-component-value?
              (component-value-of component))
            has-component-value?)))

;;;;;;
;;; unbound/presentation

(def (component e) unbound/presentation (primitive/presentation)
  ())

(def method print-component-value ((self unbound/presentation))
  #"value.unbound")

(def (function io) render-unbound-component ()
  `xml,#"value.unbound")

(def render-xhtml unbound/presentation
  (render-unbound-component))

;;;;;;
;;; null/presentation

(def (component e) null/presentation (primitive/presentation)
  ())

(def method print-component-value ((self null/presentation))
  #"value.nil")

(def (function io) render-null-component ()
  `xml,#"value.nil")

(def render-xhtml null/presentation
  (render-null-component))

;;;;;;
;;; t/read-eval-print/presentation

(def (component e) t/read-eval-print/presentation (primitive/presentation)
  ())

(def function render-t-component (component)
  (render-string-field "text" (print-component-value component) (client-state-sink-of component)))

(def method print-component-value ((component t/read-eval-print/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if has-component-value?
        (format nil "~S" component-value)
        #"value.unbound")))

(def method parse-component-value ((component t/read-eval-print/presentation) client-value)
  (if (zerop (length client-value))
      (values nil #t)
      (bind ((*read-eval* #f))
        ;; TODO: READ-FROM-STRING is kind of dangerous, even with *READ-EVAL* #f
        (values (read-from-string client-value)))))

;;;;;;
;;; boolean/presentation

(def (component e) boolean/presentation (primitive/presentation)
  ())

(def method parse-component-value ((component boolean/presentation) client-value)
  (if (string= client-value "")
      (values nil #t)
      (string-to-lisp-boolean client-value)))

;;;;;;
;;; bit/presentation

(def (component e) bit/presentation (primitive/presentation)
  ())

(def method parse-component-value ((component bit/presentation) client-value)
  (if (string= client-value "")
      (values nil #t)
      (string-to-lisp-integer client-value)))

;;;;;;
;;; character/presentation

(def (component e) character/presentation (primitive/presentation)
  ())

(def method string-field-type ((self primitive/presentation))
  "text")

(def method print-component-value ((component primitive/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (princ-to-string component-value))))

(def method parse-component-value ((component character/presentation) client-value)
  (if (string= client-value "")
      nil
      ;; KLUDGE: fix this
      (name-char (string+ "LATIN_CAPITAL_LETTER_" client-value))))

;;;;;;
;;; string/presentation

(def (component e) string/presentation (primitive/presentation)
  ())

(def method string-field-type ((self string/presentation))
  "text")

(def function render-string-component (component &key (id (generate-unique-component-id "_stw")) on-change on-key-down on-key-up)
  (render-string-field (string-field-type component)
                       (print-component-value component)
                       (client-state-sink-of component)
                       :id id
                       :on-change on-change
                       :on-key-down on-key-down
                       :on-key-up on-key-up))

(def method print-component-value ((component string/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        component-value)))

(def method parse-component-value ((component string/presentation) client-value)
  (if (string= client-value "")
      nil
      client-value))

;;;;;;
;;; password/presentation

(def (component e) password/presentation (string/presentation)
  ())

(def method string-field-type ((self password/presentation))
  "password")

;;;;;;
;;; symbol/presentation

(def (component e) symbol/presentation (string/presentation)
  ())

(def method print-component-value ((component symbol/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (fully-qualified-symbol-name component-value))))

(def method parse-component-value ((component symbol/presentation) client-value)
  ;; TODO decide what to do for uninterned symbols. currently it signals an error...
  (find-symbol* client-value))

;;;;;;
;;; keyword/presentation

(def (component e) keyword/presentation (string/presentation)
  ())

(def method print-component-value ((component keyword/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (fully-qualified-symbol-name component-value))))

(def method parse-component-value ((component keyword/presentation) client-value)
  ;; TODO decide what to do for uninterned symbols. currently it signals an error...
  (find-symbol* client-value :packages :keyword))

;;;;;;
;;; number/presentation

(def (component e) number/presentation (primitive/presentation)
  ())

(def function render-number-field-for-primitive-component (component &key (id (generate-unique-component-id "_stw")) on-change on-key-up on-key-down)
  ;; TODO was print-component-value, but spaces are not accepted as a value of the <input>
  (bind ((component-value (component-value-and-bound? component)))
    (render-number-field (if (null component-value)
                             ""
                             (princ-to-string component-value))
                         (client-state-sink-of component)
                         :id id
                         :on-change on-change
                         :on-key-up on-key-up
                         :on-key-down on-key-down)))

(def method print-component-value ((component number/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (format-number/decimal nil component-value))))

(def method parse-component-value ((component number/presentation) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
      nil
      (parse-number:parse-number client-value)))

;;;;;;
;;; real/presentation

(def (component e) real/presentation (number/presentation)
  ())

(def method parse-component-value ((component real/presentation) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-real-number client-value)))

;;;;;;
;;; complex/presentation

(def (component e) complex/presentation (number/presentation)
  ())

(def method print-component-value ((component complex/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (or (not has-component-value?)
            (null component-value))
        ""
        (bind ((imagpart (imagpart component-value)))
          (format nil "~A ~A ~Ai"
                  (format-number/decimal nil (realpart component-value))
                  (if (plusp (signum imagpart)) "+" "-")
                  (format-number/decimal nil (abs imagpart)))))))

(def method parse-component-value ((component complex/presentation) client-value)
  (if (and (length= 2 client-value)
           (not (string= (first client-value) ""))
           (not (string= (second client-value) "")))
      (complex (parse-number:parse-real-number (first client-value))
               (parse-number:parse-real-number (second client-value)))
      nil))

;;;;;;
;;; rational/presentation

(def (component e) rational/presentation (real/presentation)
  ())

;;;;;;
;;; integer/presentation

(def (component e) integer/presentation (rational/presentation)
  ())

(def render-csv integer/presentation
  (bind (((:values component-value has-component-value?) (component-value-and-bound? -self-)))
    (when (and has-component-value?
            (not (null component-value)))
      (write-csv-value (princ-to-string component-value)))))

(def method parse-component-value ((component integer/presentation) client-value)
  (if (or (string= client-value "")
          (string= client-value "NaN"))
      nil
      (values (parse-integer client-value))))

;;;;;;
;;; float/presentation

(def (component e) float/presentation (real/presentation)
  ())

;;;;;;
;;; date/presentation

(def (component e) date/presentation (primitive/presentation)
  ())

(def function render-date-component (component &key (id (generate-unique-component-id "_dtw")) on-change (printer #'print-component-value))
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

(def method print-component-value ((component date/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (and has-component-value?
             component-value)
        (print-date-value component-value)
        "")))

(def method parse-component-value ((component date/presentation) client-value)
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
;;; time/presentation

(def (component e) time/presentation (primitive/presentation)
  ())

(def function render-time-component (component &key (id (generate-unique-component-id "_tmw")) on-change (printer #'print-component-value))
  (bind (((:read-only-slots client-state-sink) component))
    (render-dojo-widget (id)
      <input (:type     "text"
              :id       ,id
              :name     ,(id-of client-state-sink)
              :constraints "{timePattern:'HH:mm:ss', clickableIncrement:'T01:00:00', visibleIncrement:'T04:00:00', visibleRange:'T12:00:00'}"
              :value    ,(funcall printer component)
              :dojoType #.+dijit/time-text-box+
              :onChange ,(force on-change))>)))

;; TODO: this prints an extra T when we simple want to print the time as a string
;;       maybe we should use dojo to localize the time value
(def function print-time-value (value)
  (local-time:format-timestring nil value :format '(#\T (:hour 2) #\: (:min 2) #\: (:sec 2)) :timezone local-time:+utc-zone+))

(def method print-component-value ((component time/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (and has-component-value?
             component-value)
        (print-time-value component-value)
        "")))

(def method parse-component-value ((component time/presentation) client-value)
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :allow-missing-date-part #t :allow-missing-timezone-part #t)
      (unless it
        (invalid-client-value "Failed to parse ~S as a time" client-value)))))

;;;;;;
;;; timestamp/presentation

(def (component e) timestamp/presentation (primitive/presentation)
  ())

(def function render-timestamp-component (component &key on-change)
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
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

(def method print-component-value ((component timestamp/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (and has-component-value?
             component-value)
        (localized-timestamp component-value)
        "")))

(def method parse-component-value ((component timestamp/presentation) client-value)
  (when (consp client-value)
    (setf client-value (apply #'string+ client-value)))
  (unless (string= client-value "")
    (aprog1 (local-time:parse-timestring client-value :fail-on-error #f)
      ;; TODO: timezone is not present in the string and thus this parsing fails: (local-time:parse-rfc3339-timestring client-value :fail-on-error #f)
      (unless it
        (invalid-client-value "Failed to parse ~S as a timestamp" client-value)))))

;;;;;;
;;; member/presentation

(def (component e) member/presentation (primitive/presentation)
  ((possible-values)
   (predicate #'equal)
   (key #'identity)
   (client-name-generator 'localized-member-component)))

(def constructor member/presentation
  (setf (possible-values-of -self-) (type-instance-list (component-value-type-of -self-))))

(def function localized-member-component (component value)
  (bind (((:values class nil slot) (extract-primitive-component-place component)))
    (localized-member-component-value class slot value)))

(def generic localized-member-component-value (class slot value)
  (:method (class slot value)
    (localized-enumeration-member value :class class :slot slot :capitalize-first-letter #t)))

(def function find-icon/member-component-value (component)
  (when (slot-boundp component 'component-value)
    (bind (((:values nil nil slot) (extract-primitive-component-place component)))
      (when slot
        (bind ((slot-name (slot-definition-name slot))
               (member-value (component-value-of component))
               (member-value-name (member-value-name/for-localization-entry member-value))
               (icon-name (string+ (string-downcase slot-name) "-" member-value-name)))
          ;; TODO use . separator, just like for localization entries
          ;; TODO find-icon should use strings
          (awhen (find-symbol* icon-name :packages (symbol-package slot-name) :otherwise nil)
            (find-icon it :otherwise nil)))))))

(def method print-component-value ((component member/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if has-component-value?
        (funcall (client-name-generator-of component) component component-value)
        "")))

(def method parse-component-value ((component member/presentation) client-value)
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
;;; html/presentation

(def (component e) html/presentation (string/presentation)
  ())

(def (function e) emit-html-string (string)
  (write-sequence (babel:string-to-octets string :encoding +default-encoding+) *xml-stream*)
  (values))

(def (function e) emit-html-component-value (component)
  (emit-html-string (print-component-value component)))

(def function render-html-component (component)
  (bind ((id (generate-unique-component-id))
         (field-id (generate-unique-component-id)))
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
;;; inet-address/presentation

(def (component e) inet-address/presentation (primitive/presentation)
  ())

(def render-xhtml inet-address/presentation
  `xml,(print-component-value -self-))

(def method print-component-value ((component inet-address/presentation))
  (bind (((:values component-value has-component-value?) (component-value-and-bound? component)))
    (if (and component-value
             has-component-value?)
        (iolib.sockets:address-to-string component-value)
        "")))

;;;;;;
;;; file/presentation

(def (component e) file/presentation (primitive/presentation)
  ())

(def (layered-function e) download-file-name (component class instance slot)
  (:method ((component file/presentation) class instance slot)
    ;; TODO wtf? fixme or delme
    (random-string)))
