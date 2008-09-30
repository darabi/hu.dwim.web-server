;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Primitive filter

(def component primitive-filter (primitive-component filter-component)
  ())

(def render :before primitive-filter
  (ensure-client-state-sink -self-))

;;;;;;
;;; T filter

(def component t-filter (t-component primitive-filter)
  ())

;;;;;;
;;; Boolean filter

(def component boolean-filter (boolean-component primitive-filter)
  ())

(def render boolean-filter ()
  <select ()
    <option "">
    ,(unless (eq (the-type-of -self-) 'boolean)
             <option ,#"value.nil">)
    <option ,#"boolean.true">
    <option ,#"boolean.false">>)

;;;;;;
;;; String filter

(def component string-filter (string-component primitive-filter)
  ((component-value nil)))

(def render string-filter ()
  (render-string-field -self-))

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
