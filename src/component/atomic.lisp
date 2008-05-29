;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Atomic

(def component atomic-component (editable-component detail-component)
  ((component-value nil)
   (allow-nil-value #f :type boolean)
   (client-state-sink nil)))

(def method render :before ((component atomic-component))
  (when (edited-p component)
    (setf (client-state-sink-of component)
          (register-client-state-sink *frame*
                                      (lambda (client-value)
                                        (setf (component-value-of component) (parse-component-value component client-value)))))))

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
  (with-slots (edited component-value client-state-sink) self
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
  (with-slots (edited component-value client-state-sink) self
    <input (:type "checkbox"
            ,(if client-state-sink (make-xml-attribute "name" (id-of client-state-sink)) +void+)
            ,(if component-value (make-xml-attribute "checked" "checked") +void+)
            ,(if edited +void+ (make-xml-attribute "disabled" "disabled")))>))

(def method parse-component-value ((component boolean-component) client-value)
  ;; TODO: browser does not send value when unchecked
  (if (string= "1" client-value)
      #t
      #f))

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

(def render string-component ()
  (with-slots (edited component-value client-state-sink) self
    (if edited
        <input (:type "text" :name (id-of client-state-sink) :value ,component-value)>
        <span ,component-value>)))

(def method parse-component-value ((component string-component) client-value)
  (if (and (allow-nil-value-p component)
           (string= client-value ""))
      nil
      client-value))

;;;;;;
;;; Number

(def component number-component (atomic-component)
  ())

(def render number-component ()
  (with-slots (edited component-value allow-nil-value client-state-sink) self
    (bind ((printed-value
            (if (and allow-nil-value
                     (null component-value))
                ""
                (princ-to-string component-value))))
      (if edited
          <input (:type "text" :name ,(id-of client-state-sink) :value ,printed-value)>
          <span ,printed-value>))))

(def method parse-component-value ((component number-component) client-value)
  (if (and (allow-nil-value-p component)
           (string= client-value ""))
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
  (if (and (allow-nil-value-p component)
           (string= client-value ""))
      nil
      (parse-integer client-value)))


;;;;;;
;;; Float

(def component float-component (number-component)
  ())

(def method parse-component-value ((component float-component) client-value)
  (if (and (allow-nil-value-p component)
           (string= client-value ""))
      nil
      (parse-number:parse-real-number client-value)))

;;;;;;
;;; Date

;; TODO:
(def component date-component (atomic-component)
  ())

;;;;;;
;;; Time

;; TODO:
(def component time-component (atomic-component)
  ())

;;;;;;
;;; Timestamp

;; TODO:
(def component timestamp-component (atomic-component)
  ())

;;;;;;
;;; Member

;; TODO:
(def component member-component (atomic-component)
  ())

;;;;;;
;;; Lisp form

;; TODO:
(def component lisp-form-component (atomic-component)
  ())

;;;;;;
;;; Lisp form

;; TODO:
(def component ip-address-component (atomic-component)
  ())
