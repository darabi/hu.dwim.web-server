;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; filter/abstract

(def (component e) filter/abstract (component-value/mixin)
  ())

;;;;;;
;;; filter/minimal

(def (component e) filter/minimal (filter/abstract component/minimal)
  ())

;;;;;;
;;; filter/basic

(def (component e) filter/basic (filter/minimal component/basic)
  ())

;;;;;;
;;; filter/style

(def (component e) filter/style (filter/basic component/style)
  ())

;;;;;;
;;; filter/full

(def (component e) filter/full (filter/style component/full)
  ())

;;;;;;
;;; Filter factory

(def layered-method make-filter (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-filter-type-for-type type))))
    (unless (subtypep component-type 'alternator/widget)
      (remove-from-plistf args :initial-alternative-type))
    (prog1-bind component (apply #'make-instance component-type
                                 ;; KLUDGE: TODO: there are both component-value and type in a filter, so which one is which?
                                 :component-value (unless (subtypep component-type 'primitive/filter)
                                                    (if (symbolp type)
                                                        (find-type-by-name type :otherwise type)
                                                        type))
                                 (append args additional-args
                                         (when (subtypep component-type 'primitive-component)
                                           (list :the-type type))))
      (when (typep component 'editable/mixin)
        (begin-editing component)))))

(def (layered-function e) find-filter-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean/filter)

  (:method ((type (eql 'components)))
    't/filter)

  (:method ((type symbol))
    (find-filter-type-for-type (find-type-by-name type)))

  (:method ((class built-in-class))
    (find-filter-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-filter-type-for-compound-type type))

  (:method ((class class))
    (find-filter-type-for-prototype (class-prototype class))))

(def function find-filter-type-for-compound-type (type)
  (find-filter-type-for-compound-type* (first type) type))

(def (layered-function e) find-filter-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member/filter :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-filter-type-for-type (first main-type))
          (find-filter-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    't/filter))

(def (layered-function e) find-filter-type-for-prototype (prototype)
  (:method ((prototype t))
    't/filter)

  (:method ((prototype string))
    'string/filter)

  (:method ((prototype symbol))
    'symbol/filter)

  (:method ((prototype integer))
    'integer/filter)

  (:method ((prototype float))
    'float/filter)

  (:method ((prototype number))
    'number/filter)

  (:method ((prototype local-time:timestamp))
    'timestamp/filter)

  (:method ((prototype structure-object))
    `(t/filter :component-value ,(class-of prototype)))

  (:method ((prototype standard-object))
    `(t/filter :component-value ,(class-of prototype))))

;;;;;;
;;; Filter interface

(def generic collect-possible-filter-predicates (component))
