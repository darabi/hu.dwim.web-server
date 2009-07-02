;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector abstract

(def (component e) inspector/abstract (editable/mixin component-value/mixin)
  ())

;;;;;;
;;; Inspector minimal

(def (component e) inspector/minimal (inspector/abstract component/minimal)
  ())

;;;;;;
;;; Inspector basic

(def (component e) inspector/basic (inspector/minimal component/basic)
  ())

;;;;;;
;;; Inspector style

(def (component e) inspector/style (inspector/basic component/style)
  ())

;;;;;;
;;; Inspector full

(def (component e) inspector/full (inspector/style component/full)
  ())

;;;;;;
;;; Inspector

(def method make-value-inspector (value)
  (make-inspector (class-of value) :component-value value))

(def method make-inspector (type &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alias refering to a parameterized or compound type such as standard-text
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parsed from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (bind (((component-type &rest additional-args)
          (ensure-list (find-inspector-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (apply #'make-instance component-type
           (append args additional-args
                   (when (subtypep component-type 'primitive/abstract)
                     (list :the-type type))))))

(def (generic e) find-inspector-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type to make an inspector for it"))

  (:method ((type (eql 'boolean)))
    'boolean/inspector)

  (:method ((type (eql 'components)))
    `(standard-object-list/inspector :the-class ,(find-class 'component)))

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
    (find-inspector-type-for-prototype (class-prototype class))))

(def (function) find-inspector-type-for-compound-type (type)
  (find-inspector-type-for-compound-type* (first type) type))

(def (generic e) find-inspector-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member/inspector :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-inspector-type-for-type (first main-type))
          (find-inspector-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'list/inspector)))

  (:method ((first (eql 'components)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'list/inspector))))

(def (function e) make-inspector-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-inspector-type-for-prototype prototype) args))

(def (generic e) find-inspector-type-for-prototype (prototype)
  (:method ((prototype t))
    't/inspector)

  (:method ((prototype string))
    'string/inspector)

  (:method ((prototype symbol))
    'symbol/inspector)

  (:method ((prototype integer))
    'integer/inspector)

  (:method ((prototype float))
    'float/inspector)

  (:method ((prototype number))
    'number/inspector)

  (:method ((prototype local-time:timestamp))
    'timestamp/inspector)

  (:method ((prototype list))
    'list/inspector)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition/inspector)

  (:method ((prototype structure-class))
    'standard-class/inspector)

  (:method ((prototype standard-class))
    'standard-class/inspector)

  (:method ((prototype structure-object))
    'standard-object/inspector)

  (:method ((prototype standard-object))
    'standard-object/inspector))
