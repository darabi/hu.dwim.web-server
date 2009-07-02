;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector

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
                   (when (subtypep component-type 'primitive-component)
                     (list :the-type type))))))

(def (generic e) find-inspector-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean/inspector)

  (:method ((type (eql 'components)))
    `(standard-object-list/inspector :the-class ,(find-class 'component)))

  (:method ((type symbol))
    (find-inspector-type-for-type (find-type-by-name type)))

  (:method ((class (eql (find-class t))))
    't/inspector)

  (:method ((class built-in-class))
    (find-inspector-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

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
          'list-component)))

  (:method ((first (eql 'components)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list/inspector :the-class ,(find-type-by-name main-type))
          'list-component))))

(def (function e) make-inspector-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-inspector-type-for-prototype prototype) args))

(def (generic e) find-inspector-type-for-prototype (prototype)
  (:method ((prototype t))
    't-maker)

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
    'list-component)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition-component)

  (:method ((prototype structure-class))
    'standard-class-component)

  (:method ((prototype standard-class))
    'standard-class-component)

  (:method ((prototype structure-object))
    'standard-object/inspector)

  (:method ((prototype standard-object))
    'standard-object/inspector))

;;;;;;
;;; Viewer

(def method make-viewer (value &key &allow-other-keys)
  (:method (value &rest args &key type &allow-other-keys)
    (remove-from-plistf args :type)
    (prog1-bind component
        (apply #'make-inspector
               (or type
                   (if (and (typep value 'proper-list)
                            (every-type-p 'standard-object value))
                       '(list standard-object)
                       (class-of value)))
               args)
      (setf (component-value-of component) value))))

;;;;;;
;;; Editor

(def (generic e) make-editor (value &key &allow-other-keys)
  (:method (value &rest args &key &allow-other-keys)
    (prog1-bind component
        (apply #'make-viewer value args)
      (begin-editing component))))

;;;;;;
;;; Filter

(def method make-filter (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-filter-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (prog1-bind component (apply #'make-instance component-type
                                 (append args additional-args
                                         (when (subtypep component-type 'primitive-component)
                                           (list :the-type type))))
      (when (typep component 'editable/mixin)
        (begin-editing component)))))

(def (generic e) find-filter-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean-filter)

  (:method ((type (eql 'components)))
    't-filter)

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

  (:method ((class structure-class))
    (find-filter-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-filter-type-for-prototype (class-prototype class))))

(def function find-filter-type-for-compound-type (type)
  (find-filter-type-for-compound-type* (first type) type))

(def (generic e) find-filter-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    `(member-filter :possible-values ,(rest type)))

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-filter-type-for-type (first main-type))
          (find-filter-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    't-filter))

(def (generic e) find-filter-type-for-prototype (prototype)
  (:method ((prototype t))
    't-filter)

  (:method ((prototype string))
    'string-filter)

  (:method ((prototype symbol))
    'symbol-filter)

  (:method ((prototype integer))
    'integer-filter)

  (:method ((prototype float))
    'float-filter)

  (:method ((prototype number))
    'number-filter)

  (:method ((prototype local-time:timestamp))
    'timestamp-filter)

  (:method ((prototype structure-object))
    `(standard-object-filter :the-class ,(class-of prototype)))

  (:method ((prototype standard-object))
    `(standard-object-filter :the-class ,(class-of prototype))))

;;;;;;
;;; Maker

(def method make-maker (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-maker-type-for-type type))))
    (unless (subtypep component-type 'alternator/basic)
      (remove-from-plistf args :initial-alternative-type))
    (prog1-bind component
        (apply #'make-instance component-type
               (append args additional-args
                       (when (subtypep component-type 'primitive-component)
                         (list :the-type type))))
      (when (typep component 'editable/mixin)
        (begin-editing component)))))

(def (generic e) find-maker-type-for-type (type)
  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'boolean-maker)

  (:method ((type (eql 'components)))
    't-maker)

  (:method ((type symbol))
    (find-maker-type-for-type (find-type-by-name type)))

  (:method ((class built-in-class))
    (find-maker-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-maker-type-for-compound-type type))

  (:method ((class structure-class))
    (find-maker-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-maker-type-for-prototype (class-prototype class))))

(def function find-maker-type-for-compound-type (type)
  (find-maker-type-for-compound-type* (first type) type))

(def (generic e) find-maker-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-maker-type-for-type (first main-type))
          (find-maker-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    't-maker))

(def (generic e) find-maker-type-for-prototype (prototype)
  (:method ((prototype t))
    't-maker)

  (:method ((prototype string))
    'string-maker)

  (:method ((prototype symbol))
    'symbol-maker)

  (:method ((prototype integer))
    'integer-maker)

  (:method ((prototype float))
    'float-maker)

  (:method ((prototype number))
    'number-maker)

  (:method ((prototype local-time:timestamp))
    'timestamp-maker)

  (:method ((instance structure-object))
    `(standard-object-maker :the-class ,(class-of instance)))

  (:method ((instance standard-object))
    `(standard-object-maker :the-class ,(class-of instance))))

;;;;;;
;;; Place inspector

(def (function e) make-place-inspector (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-inspector-type-for-type type))))
    (apply #'make-instance component-type (append args additional-args))))

(def (function e) make-special-variable-place-inspector (name type)
  (make-place-inspector type :place (make-special-variable-place name type)))

(def (macro e) make-lexical-variable-place-inspector (name type)
  `(make-place-inspector ,type :place (make-lexical-variable-place ,name ,type)))

(def (function e) make-standard-object-slot-value-place-inspector (instance slot)
  (bind ((place (make-slot-value-place instance slot)))
    (make-place-inspector (place-type place) :place place)))

(def (generic e) find-place-inspector-type-for-type (type)
  (:method (type)
    'place/inspector)

  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'place/inspector)

  (:method ((type (eql 'components)))
    'standard-object-place/inspector)
  
  (:method ((type symbol))
    (find-place-inspector-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-inspector-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-inspector-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-inspector-type-for-prototype (class-prototype class))))

(def function find-place-inspector-type-for-compound-type (type)
  (find-place-inspector-type-for-compound-type* (first type) type))

(def (generic e) find-place-inspector-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-inspector-type-for-type (first main-type))
          (find-place-inspector-type-for-type t))))

  (:method ((first (eql 'components)) (type cons))
    (find-place-inspector-type-for-type (second type))))

(def (generic e) find-place-inspector-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place/inspector)

  (:method ((prototype standard-object))
    'standard-object-place/inspector))

;;;;;;
;;; Place maker

(def (function e) make-place-maker (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-maker-type-for-type type))))
    (apply #'make-instance component-type :the-type type (append args additional-args))))

(def (generic e) find-place-maker-type-for-type (type)
  (:method (type)
    'place-maker)

  (:method ((type null))
    (error "NIL is not a valid type here"))
  
  (:method ((type (eql 'boolean)))
    'place-maker)
  
  (:method ((type symbol))
    (find-place-maker-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-maker-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-maker-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-maker-type-for-prototype (class-prototype class))))

(def function find-place-maker-type-for-compound-type (type)
  (find-place-maker-type-for-compound-type* (first type) type))

(def (generic e) find-place-maker-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-maker-type-for-type (first main-type))
          (find-place-maker-type-for-type t)))))

(def (generic e) find-place-maker-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place-maker)

  (:method ((prototype standard-object))
    'standard-object-place-maker))

;;;;;;
;;; Place filter

(def (function e) make-place-filter (type &rest args)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-place-filter-type-for-type type))))
    (apply #'make-instance component-type :the-type type (append args additional-args))))

(def (generic e) find-place-filter-type-for-type (type)
  (:method (type)
    'place-filter)

  (:method ((type null))
    (error "NIL is not a valid type here"))

  (:method ((type (eql 'boolean)))
    'place-filter)
  
  (:method ((type symbol))
    (find-place-filter-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-place-filter-type-for-compound-type type))

  (:method ((class structure-class))
    (find-place-filter-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-place-filter-type-for-prototype (class-prototype class))))

(def function find-place-filter-type-for-compound-type (type)
  (find-place-filter-type-for-compound-type* (first type) type))

(def (generic e) find-place-filter-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    'place-filter)

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-place-filter-type-for-type (first main-type))
          (find-place-filter-type-for-type t)))))

(def (generic e) find-place-filter-type-for-prototype (prototype)
  (:method ((prototype structure-object))
    'standard-object-place-filter)

  (:method ((prototype standard-object))
    'standard-object-place-filter))

;;;;;;
;;; Utility

;; TODO: KLUDGE: is this really this simple?
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or null)))
             type))

(def function null-subtype-p (type)
  (subtypep 'null type))
