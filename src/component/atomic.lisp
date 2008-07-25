;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; TODO the way widgets are rendered needs to be factored more

;;;;;;
;;; Atomic

(def component atomic-component (editable-component detail-component)
  ((component-value nil)
   (allow-nil-value #f :type boolean)
   (client-state-sink nil)))

(def render :before atomic-component
  (when (edited-p -self-)
    (setf (client-state-sink-of -self-)
          (register-client-state-sink *frame*
                                      (lambda (client-value)
                                        (setf (component-value-of -self-) (parse-component-value -self- client-value)))))))

(def generic parse-component-value (component client-value))

;;;;;;
;;; Unbound

(def component unbound-component (atomic-component)
  ())

(def render unbound-component ()
  <span "UNBOUND">)

;;;;;;
;;; Null

(def component null-component (atomic-component)
  ())

(def render null-component ()
  <span "NIL">)

;;;;;;
;;; T
;;;
;;; Viewer:
;;;   unbound -> <unbound-marker>
;;;   nil     -> NIL
;;;   ""      -> ""
;;;   "NIL"   -> "NIL"
;;;   42      -> 42
;;; Editor:
;;;   unbound -> <unbound-marker> []
;;;   nil     -> [NIL]
;;;   ""      -> [""]
;;;   "NIL"   -> ["NIL"]
;;;   42      -> [42]

(def component t-component (atomic-component)
  ())

(def render t-component ()
  (with-slots (edited component-value client-state-sink) -self-
    (bind ((printed-value (format nil "~S" component-value)))
      (if edited
          <input (:type "text" :name ,(id-of client-state-sink) :value ,printed-value)>
          <span ,printed-value>))))

(def method parse-component-value ((component t-component) client-value)
  ;; TODO: this is kind of dangerous
  (eval (read-from-string client-value)))

;;;;;;
;;; Boolean
;;;
;;; Type: boolean
;;; Viewer:
;;;   #f -> [ ]
;;;   #t -> [x]
;;; Editor:
;;;   #f -> [ ]
;;;   #t -> [x]
;;;
;;; Type: (or unbound boolean)
;;; Viewer:
;;;   unbound -> <unbound-marker>
;;;   #f      -> [ ]
;;;   #t      -> [x]
;;; Editor:
;;;   unbound -> <unbound-marker> [ ]
;;;   #f      -> [ ]
;;;   #t      -> [x]

(def component boolean-component (atomic-component)
  ())

(def render boolean-component ()
  (with-slots (edited component-value client-state-sink) -self-
    <div
      ,(if edited
           <input (:type "hidden" :name ,(id-of client-state-sink) :value ,(if component-value "#t" "#f"))>
           +void+)
      <input (:type "checkbox"
              ,(if component-value (make-xml-attribute "checked" "checked") +void+)
              ,(if edited +void+ (make-xml-attribute "disabled" "disabled")))>>))

(def method parse-component-value ((component boolean-component) client-value)
  (cond ((string= "#t" client-value)
         #t)
        ((string= "#f" client-value)
         #f)
        (t (error "Unknown boolean value ~A" client-value))))

;;;;;;
;;; String
;;;
;;; Type: string
;;; Viewer:
;;;   ""      ->
;;;   "NIL"   -> NIL
;;;   "Hello" -> Hello
;;; Editor:
;;;   ""      -> []
;;;   "NIL"   -> [NIL]
;;;   "Hello" -> [Hello]
;;;
;;; Type: (or null string)
;;; Optionally "" may be treated as nil.
;;; Viewer:
;;;   nil     -> <nil marker>
;;;   ""      ->
;;;   ""      -> <nil marker>
;;;   "NIL"   -> NIL
;;;   "Hello" -> Hello
;;; Editor:
;;;   nil     -> <nil marker> []
;;;   ""      -> []
;;;   ""      -> <nil marker> []
;;;   "NIL"   -> [NIL]
;;;   "Hello" -> [Hello]

(def component string-component (atomic-component)
  ())

(def function render-string-component (self type)
  (with-slots (edited component-value allow-nil-value client-state-sink) self
    (bind ((printed-value (if (null component-value)
                              ""
                              component-value))
           (id (generate-frame-unique-string "_w")))
      (if edited
          (render-dojo-widget (id)
            ;; TODO dojoRows  3
            <input (:type      ,type
                    :id        ,id
                    :name      ,(id-of client-state-sink)
                    :value     ,printed-value
                    dojoType  #.+dijit/text-box+)>)
          <span ,printed-value>))))

(def render string-component ()
  (render-string-component -self- "text"))

(def method parse-component-value ((component string-component) client-value)
  (if (and (allow-nil-value-p component)
           (string= client-value ""))
      nil
      client-value))

;;;;;;
;;; Password

(def component password-component (string-component)
  ())

(def render password-component ()
  (render-string-component -self- "password"))

;;;;;;
;;; Symbol

(def component symbol-component (string-component)
  ())

;;;;;;
;;; Number

(def component number-component (atomic-component)
  ())

(def render number-component ()
  (with-slots (edited component-value allow-nil-value client-state-sink) -self-
    (bind ((printed-value (if (null component-value)
                              ""
                              (princ-to-string component-value)))
           (id (generate-frame-unique-string "_w")))
      (if edited
          (render-dojo-widget (id)
            <input (:type      "text"
                    :id        ,id
                    :name      ,(id-of client-state-sink)
                    :value     ,printed-value
                    :dojoType  #.+dijit/number-text-box+)>)
          <span ,printed-value>))))

(def method parse-component-value ((component number-component) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-number client-value)))

;;;;;;
;;; Integer
;;;
;;; Type: integer
;;; Viewer:
;;;   42  -> 42
;;; Editor:
;;;   42  -> [42]
;;;
;;; Type: (or null integer)
;;; Viewer:
;;;   nil -> <nil marker>
;;;   42  -> 42
;;; Editor:
;;;   nil -> []
;;;   42  -> [42]
;;;
;;; Type: (or unbound integer)
;;; Viewer:
;;;   unbound -> <unbound-marker>
;;;   42      -> 42
;;; Editor:
;;;   unbound -> <unbound-marker> []
;;;   42      -> [42]

(def component integer-component (number-component)
  ())

(def method parse-component-value ((component integer-component) client-value)
  (if (string= client-value "")
      nil
      (parse-integer client-value)))


;;;;;;
;;; Float

(def component float-component (number-component)
  ())

(def method parse-component-value ((component float-component) client-value)
  (if (string= client-value "")
      nil
      (parse-number:parse-real-number client-value)))

;;;;;;
;;; Date

(def component date-component (atomic-component)
  ())

(def render date-component ()
  (with-slots (edited component-value allow-nil-value client-state-sink) -self-
    (bind ((wrong-value? (and (not allow-nil-value)
                              (not component-value)))
           (printed-value (or (when component-value
                                (local-time:format-rfc3339-timestring nil component-value))
                              (if (or allow-nil-value
                                      edited)
                                  ""
                                  #"wrong-atomic-component-value")))
           (id (generate-frame-unique-string "_w")))
      (if edited
          (render-dojo-widget (id)
            <input (:type      "text"
                    :id        ,id
                    :name      ,(id-of client-state-sink)
                    :value     ,printed-value
                    :dojoType  #.+dijit/date-text-box+)>)
          <span (:class ,(when wrong-value? "wrong"))
            ,printed-value>))))

(def method parse-component-value ((component date-component) client-value)
  (unless (string= client-value "")
    (bind ((result (local-time:parse-rfc3339-timestring client-value :allow-missing-time-part #t)))
      (local-time:with-decoded-timestamp (:hour hour :minute minute :sec sec :nsec nsec) result
        (unless (and (zerop hour)
                     (zerop minute)
                     (zerop sec)
                     (zerop nsec))
          ;; TODO add validation error
          (setf result nil)))
      result)))

;;;;;;
;;; Time

(def component time-component (atomic-component)
  ())

(def render time-component ()
  (with-slots (edited component-value allow-nil-value client-state-sink) -self-
    (bind ((wrong-value? (and (not allow-nil-value)
                              (not component-value)))
           (printed-value (or (when component-value
                                (local-time:format-rfc3339-timestring nil component-value :omit-date-part #t))
                              (if (or allow-nil-value
                                      edited)
                                  ""
                                  #"wrong-atomic-component-value")))
           (id (generate-frame-unique-string "_w")))
      (if edited
          (render-dojo-widget (id)
            <input (:type      "text"
                    :id        ,id
                    :name      ,(id-of client-state-sink)
                    :value     ,printed-value
                    :dojoType  #.+dijit/time-text-box+)>)
          <span (:class ,(when wrong-value? "wrong"))
            ,printed-value>))))

;;;;;;
;;; Timestamp

;; TODO:
(def component timestamp-component (atomic-component)
  ())

(def render timestamp-component ()
  <span "TODO: timestamp-component">)

(def method parse-component-value ((component timestamp-component) client-value)
  (unless (string= client-value "")
    ;; TODO check for errors, add user message
    (local-time:parse-rfc3339-timestring client-value)))

;;;;;;
;;; Member

(def component member-component (atomic-component style-component-mixin remote-identity-component-mixin)
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

(def render member-component ()
  (bind (((:read-only-slots component-value) -self-))
    (if (edited-p -self-)
        (bind ((id (id-of -self-))
               ((:read-only-slots possible-values allow-nil-value comparator key client-name-generator) -self-))
          (render-dojo-widget (id)
            <select (:id       ,id
                     :dojoType #.+dijit/filtering-select+
                     :name     ,(id-of (client-state-sink-of -self-)))
              ,@(iter (for index :upfrom 0)
                      (generate el :in-sequence possible-values)
                      (for possible-value = (funcall key (if (first-time-p)
                                                             (if allow-nil-value
                                                                 nil
                                                                 (next el))
                                                             (next el))))
                      (for client-name = (funcall client-name-generator possible-value))
                      (for client-value = (integer-to-string index))
                      (collect <option (:value ,client-value
                                        ,(when (funcall comparator component-value possible-value)
                                           (make-xml-attribute "selected" "yes")))
                                 ,client-name>))>))
        <span ,(princ-to-string component-value)>)))

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

;;;;;;
;;; Lisp form

;; TODO:
(def component lisp-form-component (atomic-component)
  ())

(def render lisp-form-component ()
  <span "TODO: lisp-form-component">)

;;;;;;
;;; Lisp form

;; TODO:
(def component ip-address-component (atomic-component)
  ())

(def render ip-address-component ()
  <span "TODO: ip-address-component">)
