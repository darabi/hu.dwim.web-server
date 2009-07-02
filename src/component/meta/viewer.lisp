;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Viewer abstract

(def (component e) viewer/abstract (component-value/mixin)
  ())

;;;;;;
;;; Viewer minimal

(def (component e) viewer/minimal (viewer/abstract component/minimal)
  ())

;;;;;;
;;; Viewer basic

(def (component e) viewer/basic (viewer/minimal component/basic)
  ())

;;;;;;
;;; Viewer style

(def (component e) viewer/style (viewer/basic component/style)
  ())

;;;;;;
;;; Viewer full

(def (component e) viewer/full (viewer/style component/full)
  ())

;;;;;;
;;; Viewer

(def methods make-value-viewer
  (:method (value)
    (make-viewer (class-of value) :component-value value))

  (:method ((value number))
    value)

  (:method ((value string))
    value))

(def method make-viewer (type &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alias refering to a parameterized or compound type such as standard-text
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parsed from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (bind (((component-type &rest additional-args)
          (ensure-list (find-viewer-type-for-type type)))
         (component-class (find-type-by-name component-type)))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (if (typep component-class 'built-in-class)
        (class-prototype component-class)
        (apply #'make-instance component-type
               (append args additional-args
                       (when (subtypep component-type 'primitive/abstract)
                         (list :the-type type)))))))

(def (generic e) find-viewer-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type to make a viewer for it"))

  (:method ((type (eql 'boolean)))
    'boolean/viewer)

  (:method ((type (eql 'components)))
    `(standard-object-list/viewer :the-class ,(find-class 'component)))

  (:method ((type symbol))
    (find-viewer-type-for-type (find-type-by-name type)))

  (:method ((class (eql (find-class t))))
    't/viewer)

  (:method ((class built-in-class))
    (find-viewer-type-for-prototype (class-prototype class)))

  (:method ((type cons))
    (find-viewer-type-for-compound-type type))

  (:method ((class structure-class))
    (find-viewer-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-viewer-type-for-prototype (class-prototype class))))

(def (function) find-viewer-type-for-compound-type (type)
  (find-viewer-type-for-compound-type* (first type) type))

(def (generic e) find-viewer-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member/viewer :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-viewer-type-for-type (first main-type))
          (find-viewer-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/viewer :the-class ,(find-type-by-name main-type))
          'list/viewer)))

  (:method ((first (eql 'components)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/viewer :the-class ,(find-type-by-name main-type))
          'list/viewer))))

(def (function e) make-viewer-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-viewer-type-for-prototype prototype) args))

(def (generic e) find-viewer-type-for-prototype (prototype)
  (:method ((prototype t))
    't/viewer)

  (:method ((prototype string))
    'string)

  (:method ((prototype symbol))
    'symbol/viewer)

  (:method ((prototype integer))
    'integer)

  (:method ((prototype float))
    'float)

  (:method ((prototype number))
    'number)

  (:method ((prototype local-time:timestamp))
    'timestamp/viewer)

  (:method ((prototype list))
    'list/viewer)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition/viewer)

  (:method ((prototype structure-class))
    'standard-class/viewer)

  (:method ((prototype standard-class))
    'standard-class/viewer)

  (:method ((prototype structure-object))
    'standard-object/viewer)

  (:method ((prototype standard-object))
    'standard-object/viewer))
