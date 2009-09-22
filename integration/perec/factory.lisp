;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector

(def methods find-inspector-type-for-type
  (:method ((type hu.dwim.perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type hu.dwim.perec:boolean-type))
    'boolean-inspector)

  (:method ((type hu.dwim.perec:integer-type))
    'integer-inspector)

  (:method ((type hu.dwim.perec:float-type))
    'float-inspector)

  (:method ((type hu.dwim.perec:float-32-type))
    'float-inspector)

  (:method ((type hu.dwim.perec:float-64-type))
    'float-inspector)

  (:method ((type hu.dwim.perec:number-type))
    'number-inspector)

  (:method ((type hu.dwim.perec:string-type))
    'string-inspector)

  (:method ((type hu.dwim.meta-model:html-text-type))
    'html-inspector)

  (:method ((type hu.dwim.perec:date-type))
    'date-inspector)

  (:method ((type hu.dwim.perec:time-type))
    'time-inspector)

  (:method ((type hu.dwim.perec:timestamp-type))
    'timestamp-inspector)

  (:method ((type hu.dwim.perec:member-type))
    (list 'member-inspector :possible-values (hu.dwim.perec:members-of type)))

  (:method ((type hu.dwim.perec:form-type))
    'expression-inspector)

  (:method ((type hu.dwim.perec:serialized-type))
    't-inspector)

  (:method ((type hu.dwim.perec:set-type))
    `(standard-object-list-inspector :the-class ,(hu.dwim.perec::element-type-of type)))

  (:method ((type hu.dwim.perec:ip-address-type))
    'ip-address-inspector))

(def method find-inspector-type-for-compound-type* (first type)
  (find-inspector-type-for-type (hu.dwim.perec:parse-type type)))

(def method find-inspector-type-for-prototype ((prototype hu.dwim.perec::d-value))
  'd-value-inspector)

;;;;;;
;;; Filter

(def methods find-filter-type-for-type
  (:method ((type hu.dwim.perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type hu.dwim.perec:boolean-type))
    'boolean-filter)

  (:method ((type hu.dwim.perec:integer-type))
    'integer-filter)

  (:method ((type hu.dwim.perec:float-type))
    'float-filter)

  (:method ((type hu.dwim.perec:float-32-type))
    'float-filter)

  (:method ((type hu.dwim.perec:float-64-type))
    'float-filter)

  (:method ((type hu.dwim.perec:number-type))
    'number-filter)

  (:method ((type hu.dwim.perec:string-type))
    'string-filter)

  (:method ((type hu.dwim.meta-model:html-text-type))
    'html-filter)

  (:method ((type hu.dwim.perec:date-type))
    'date-filter)

  (:method ((type hu.dwim.perec:time-type))
    'time-filter)

  (:method ((type hu.dwim.perec:timestamp-type))
    'timestamp-filter)

  (:method ((type hu.dwim.perec:member-type))
    (list 'member-filter :possible-values (hu.dwim.perec:members-of type)))

  (:method ((type hu.dwim.perec:form-type))
    'expression-filter)

  (:method ((type hu.dwim.perec:serialized-type))
    't-filter)

  (:method ((type hu.dwim.perec:ip-address-type))
    'ip-address-filter)

  (:method ((type hu.dwim.perec:set-type))
    ;; TODO:
    't-filter))

(def method find-filter-type-for-compound-type* (first type)
  (find-filter-type-for-type (hu.dwim.perec:parse-type type)))

;;;;;;
;;; Maker

(def methods find-maker-type-for-type
  (:method ((type hu.dwim.perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type hu.dwim.perec:boolean-type))
    'boolean-maker)

  (:method ((type hu.dwim.perec:integer-type))
    'integer-maker)

  (:method ((type hu.dwim.perec:float-type))
    'float-maker)

  (:method ((type hu.dwim.perec:float-32-type))
    'float-maker)

  (:method ((type hu.dwim.perec:float-64-type))
    'float-maker)

  (:method ((type hu.dwim.perec:number-type))
    'number-maker)

  (:method ((type hu.dwim.perec:string-type))
    'string-maker)

  (:method ((type hu.dwim.meta-model:html-text-type))
    'html-maker)

  (:method ((type hu.dwim.perec:date-type))
    'date-maker)

  (:method ((type hu.dwim.perec:time-type))
    'time-maker)

  (:method ((type hu.dwim.perec:timestamp-type))
    'timestamp-maker)

  (:method ((type hu.dwim.perec:member-type))
    (list 'member-maker :possible-values (hu.dwim.perec:members-of type)))

  (:method ((type hu.dwim.perec:form-type))
    'expression-maker)

  (:method ((type hu.dwim.perec:serialized-type))
    't-maker)

  (:method ((type hu.dwim.perec:ip-address-type))
    'ip-address-maker)

  (:method ((type hu.dwim.perec:set-type))
    `(standard-object-list-inspector :the-class ,(hu.dwim.perec::element-type-of type))))

(def method find-maker-type-for-compound-type* (first type)
  (find-maker-type-for-type (hu.dwim.perec:parse-type type)))

;;;;;;
;;; Place inspector

(def method find-place-inspector-type-for-compound-type* (first type)
  (find-place-inspector-type-for-type (hu.dwim.perec:parse-type type)))

(def method find-place-inspector-type-for-type ((type (eql 'hu.dwim.perec::timestamp)))
  'place-inspector)

;;;;;;
;;; Place maker

(def method find-place-maker-type-for-compound-type* (first type)
  (find-place-maker-type-for-type (hu.dwim.perec:parse-type type)))

(def method find-place-maker-type-for-type ((type (eql 'hu.dwim.perec::timestamp)))
  'place-maker)

;;;;;;
;;; Place filter

(def method find-place-filter-type-for-compound-type* (first type)
  (find-place-filter-type-for-type (hu.dwim.perec:parse-type type)))

(def method find-place-filter-type-for-type ((type (eql 'hu.dwim.perec::timestamp)))
  'place-filter)