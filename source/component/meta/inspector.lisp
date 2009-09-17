;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; inspector/abstract

(def (component e) inspector/abstract (editable/mixin component-value/mixin)
  ())

;;;;;;
;;; inspector/minimal

(def (component e) inspector/minimal (inspector/abstract component/minimal)
  ())

;;;;;;
;;; inspector/basic

(def (component e) inspector/basic (inspector/minimal component/basic)
  ())

;;;;;;
;;; inspector/style

(def (component e) inspector/style (inspector/basic component/style)
  ())

;;;;;;
;;; inspector/full

(def (component e) inspector/full (inspector/style component/full)
  ())

;;;;;;
;;; Inspector factory

(def layered-method make-inspector (type value &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alias refering to a parameterized or compound type such as standard-text
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parsed from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (bind (((component-type &rest additional-args)
          (ensure-list (find-inspector-type-for-type type))))
    (unless (subtypep component-type 'alternator/widget)
      (remove-from-plistf args :initial-alternative-type))
    (apply #'make-instance component-type
           :component-value value
           (append args additional-args
                   (when (subtypep component-type 'primitive/abstract)
                     (list :the-type type))))))

(def (layered-function e) find-inspector-type-for-type (type)
  (:method (class)
    't/inspector)
  
  (:method ((type null))
    (error "NIL is not a valid type to make an inspector for it"))

  (:method ((type (eql 'boolean)))
    'boolean/inspector)

  (:method ((type (eql 'components)))
    `(sequence/inspector :the-class ,(find-class 'component)))

  (:method ((type symbol))
    (find-inspector-type-for-type (find-type-by-name type)))

  (:method ((class (eql (find-class t))))
    't/inspector)

  (:method ((class built-in-class))
    (find-inspector-type-for-prototype (class-prototype class)))

  (:method ((type cons))
    (find-inspector-type-for-compound-type type))

  (:method ((class structure-class))
    (find-inspector-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-inspector-type-for-prototype (class-prototype class)))

  (:method ((class funcallable-standard-class))
    (find-inspector-type-for-prototype (class-prototype class))))

(def (function) find-inspector-type-for-compound-type (type)
  (find-inspector-type-for-compound-type* (first type) type))

(def (layered-function e) find-inspector-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member/inspector :possible-values ,(rest type)))

  (:method ((first (eql 'single-float)) (type cons))
    'float/inspector)

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-inspector-type-for-type (first main-type))
          (find-inspector-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'sequence/inspector)))

  (:method ((first (eql 'components)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'sequence/inspector))))

(def (function e) make-inspector-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-inspector-type-for-prototype prototype) args))

;; TODO: split all around?
(def (layered-function e) find-inspector-type-for-prototype (prototype)
  (:method ((prototype t))
    't/inspector)

  (:method ((prototype string))
    'string/inspector)

  (:method ((prototype symbol))
    (if (null prototype)
        (call-next-method)
        'symbol/inspector))

  (:method ((prototype integer))
    'integer/inspector)

  (:method ((prototype float))
    'float/inspector)

  (:method ((prototype number))
    'number/inspector)

  (:method ((prototype local-time:timestamp))
    'timestamp/inspector)

  (:method ((prototype list))
    'sequence/inspector)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition/inspector)

  (:method ((prototype structure-object))
    't/inspector)

  (:method ((prototype package))
    'package/inspector)

  (:method ((prototype standard-object))
    't/inspector))

(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or null)))
             type))
