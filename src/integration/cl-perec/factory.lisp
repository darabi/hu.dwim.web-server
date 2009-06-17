;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector

(def methods find-inspector-type-for-type
  (:method ((type cl-perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type cl-perec:boolean-type))
    'boolean-inspector)

  (:method ((type cl-perec:integer-type))
    'integer-inspector)

  (:method ((type cl-perec:float-type))
    'float-inspector)

  (:method ((type cl-perec:float-32-type))
    'float-inspector)

  (:method ((type cl-perec:float-64-type))
    'float-inspector)

  (:method ((type cl-perec:number-type))
    'number-inspector)

  (:method ((type cl-perec:string-type))
    'string-inspector)

  (:method ((type dmm:html-text-type))
    'html-inspector)

  (:method ((type cl-perec:date-type))
    'date-inspector)

  (:method ((type cl-perec:time-type))
    'time-inspector)

  (:method ((type cl-perec:timestamp-type))
    'timestamp-inspector)

  (:method ((type cl-perec:member-type))
    (list 'member-inspector :possible-values (cl-perec:members-of type)))

  (:method ((type cl-perec:form-type))
    'expression-inspector)

  (:method ((type cl-perec:serialized-type))
    't-inspector)

  (:method ((type cl-perec:set-type))
    `(standard-object-list-inspector :the-class ,(prc::element-type-of type)))

  (:method ((type cl-perec:ip-address-type))
    'ip-address-inspector))

(def method find-inspector-type-for-compound-type* (first type)
  (find-inspector-type-for-type (cl-perec:parse-type type)))

(def method find-inspector-type-for-prototype ((prototype prc::d-value))
  'd-value-inspector)

;;;;;;
;;; Filter

(def methods find-filter-type-for-type
  (:method ((type cl-perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type cl-perec:boolean-type))
    'boolean-filter)

  (:method ((type cl-perec:integer-type))
    'integer-filter)

  (:method ((type cl-perec:float-type))
    'float-filter)

  (:method ((type cl-perec:float-32-type))
    'float-filter)

  (:method ((type cl-perec:float-64-type))
    'float-filter)

  (:method ((type cl-perec:number-type))
    'number-filter)

  (:method ((type cl-perec:string-type))
    'string-filter)

  (:method ((type dmm:html-text-type))
    'html-filter)

  (:method ((type cl-perec:date-type))
    'date-filter)

  (:method ((type cl-perec:time-type))
    'time-filter)

  (:method ((type cl-perec:timestamp-type))
    'timestamp-filter)

  (:method ((type cl-perec:member-type))
    (list 'member-filter :possible-values (cl-perec:members-of type)))

  (:method ((type cl-perec:form-type))
    'expression-filter)

  (:method ((type cl-perec:serialized-type))
    't-filter)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-filter)

  (:method ((type cl-perec:set-type))
    ;; TODO:
    't-filter))

(def method find-filter-type-for-compound-type* (first type)
  (find-filter-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Maker

(def methods find-maker-type-for-type
  (:method ((type cl-perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type cl-perec:boolean-type))
    'boolean-maker)

  (:method ((type cl-perec:integer-type))
    'integer-maker)

  (:method ((type cl-perec:float-type))
    'float-maker)

  (:method ((type cl-perec:float-32-type))
    'float-maker)

  (:method ((type cl-perec:float-64-type))
    'float-maker)

  (:method ((type cl-perec:number-type))
    'number-maker)

  (:method ((type cl-perec:string-type))
    'string-maker)

  (:method ((type dmm:html-text-type))
    'html-maker)

  (:method ((type cl-perec:date-type))
    'date-maker)

  (:method ((type cl-perec:time-type))
    'time-maker)

  (:method ((type cl-perec:timestamp-type))
    'timestamp-maker)

  (:method ((type cl-perec:member-type))
    (list 'member-maker :possible-values (cl-perec:members-of type)))

  (:method ((type cl-perec:form-type))
    'expression-maker)

  (:method ((type cl-perec:serialized-type))
    't-maker)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-maker)

  (:method ((type cl-perec:set-type))
    `(standard-object-list-inspector :the-class ,(prc::element-type-of type))))

(def method find-maker-type-for-compound-type* (first type)
  (find-maker-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Place inspector

(def method find-place-inspector-type-for-compound-type* (first type)
  (find-place-inspector-type-for-type (cl-perec:parse-type type)))

(def method find-place-inspector-type-for-type ((type (eql 'prc::timestamp)))
  'place-inspector)

;;;;;;
;;; Place maker

(def method find-place-maker-type-for-compound-type* (first type)
  (find-place-maker-type-for-type (cl-perec:parse-type type)))

(def method find-place-maker-type-for-type ((type (eql 'prc::timestamp)))
  'place-maker)

;;;;;;
;;; Place filter

(def method find-place-filter-type-for-compound-type* (first type)
  (find-place-filter-type-for-type (cl-perec:parse-type type)))

(def method find-place-filter-type-for-type ((type (eql 'prc::timestamp)))
  'place-filter)
