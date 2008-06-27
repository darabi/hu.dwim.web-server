;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def function find-type-by-name (name)
  ;; TODO this redefines, but we are practical for now
  (or (find-class name #f)
      (cl-perec:find-type name)))

(def methods find-inspector-component-type-for-type
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
    'standard-object-list-component)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-component))

(def method find-inspector-component-type-for-compound-type* (first type)
  (find-inspector-component-type-for-type (cl-perec:parse-type type)))


;;;;;;
;;; Filter

(def methods find-filter-component-type-for-type
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
    (list 'member-component :possible-values (cl-perec:members-of type) :allow-nil-value #t))

  (:method ((type cl-perec:serialized-type))
    't-component)

  (:method ((type cl-perec:ip-address-type))
    'ip-address-component)

  (:method ((type cl-perec:set-type))
    `(label-component :component-value "TODO")))

(def method find-filter-component-type-for-compound-type* (first type)
  (find-filter-component-type-for-type (cl-perec:parse-type type)))


;;;;;;
;;; Maker

(def method find-maker-component-type-for-compound-type* (first type)
  (find-maker-component-type-for-type (cl-perec:parse-type type)))


;;;;;;
;;; Utility

;; TODO: KLUDGE:
;; TODO this redefines, but we are practical for now
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or cl-perec:unbound null)))
             type))
