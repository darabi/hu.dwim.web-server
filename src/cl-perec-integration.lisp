;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Utility

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or cl-perec:unbound null)))
             type))

;; TODO: KLUDGE: this redefines, but we are practical for now
(def function find-type-by-name (name)
  (or (find-class name #f)
      (cl-perec:find-type name)))

;;;;;;
;;; Inspector

(def methods find-inspector-type-for-type
  (:method ((type cl-perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type cl-perec:boolean-type))
    'boolean-component)

  (:method ((type cl-perec:integer-type))
    'integer-component)

  (:method ((type cl-perec:float-type))
    'float-component)

  (:method ((type cl-perec:float-32-type))
    'float-component)

  (:method ((type cl-perec:float-64-type))
    'float-component)

  (:method ((type cl-perec:number-type))
    'number-component)

  (:method ((type cl-perec:text-type))
    'string-component)

  (:method ((type cl-perec:date-type))
    'date-component)

  (:method ((type cl-perec:time-type))
    'time-component)

  (:method ((type cl-perec:timestamp-type))
    'timestamp-component)

  (:method ((type cl-perec:member-type))
    (list 'member-component :possible-values (cl-perec:members-of type)))

  (:method ((type cl-perec:serialized-type))
    't-component)

  (:method ((type cl-perec:set-type))
    'standard-object-list-inspector)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-component))

(def method find-inspector-type-for-compound-type* (first type)
  (find-inspector-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Filter

(def methods find-filter-type-for-type
  (:method ((type cl-perec:persistent-type))
    (error "Unknown type ~A" type))

  (:method ((type cl-perec:boolean-type))
    'boolean-component)

  (:method ((type cl-perec:integer-type))
    'integer-component)

  (:method ((type cl-perec:float-type))
    'float-component)

  (:method ((type cl-perec:float-32-type))
    'float-component)

  (:method ((type cl-perec:float-64-type))
    'float-component)

  (:method ((type cl-perec:number-type))
    'number-component)

  (:method ((type cl-perec:text-type))
    'string-component)

  (:method ((type cl-perec:date-type))
    'date-component)

  (:method ((type cl-perec:time-type))
    'time-component)

  (:method ((type cl-perec:timestamp-type))
    'timestamp-component)

  (:method ((type cl-perec:member-type))
    (list 'member-component :possible-values (cl-perec:members-of type)))

  (:method ((type cl-perec:serialized-type))
    't-component)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-component)

  (:method ((type cl-perec:set-type))
    `(label-component :component-value "TODO")))

(def method find-filter-type-for-compound-type* (first type)
  (find-filter-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Maker

(def method find-maker-type-for-compound-type* (first type)
  (find-maker-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Place inspector

(def method find-place-inspector-type-for-compound-type* (first type)
  (find-place-inspector-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Place maker

(def method find-place-maker-type-for-compound-type* (first type)
  (find-place-maker-type-for-type (cl-perec:parse-type type)))

;;;;;;
;;; Place filter

(def method find-place-filter-type-for-compound-type* (first type)
  (find-place-filter-type-for-type (cl-perec:parse-type type)))
