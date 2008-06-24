;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (function e) make-inspector-component (type &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alias refering to a parameterized or compound type such as standard-text
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parsed from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (bind (((component-type &rest additional-args)
          (ensure-list (find-inspector-component-type-for-type type))))
    (unless (subtypep component-type 'alternator-component)
      (remove-from-plistf args :default-component-type))
    (apply #'make-instance component-type (append args additional-args))))

(def function find-type-by-name (name)
  (find-class name #f))

(def (generic e) find-inspector-component-type-for-type (type)
  (:method ((type symbol))
    (find-inspector-component-type-for-type (find-type-by-name type)))

  (:method ((type (eql 'boolean)))
    'boolean-component)

  (:method ((class (eql (find-class t))))
    't-component)

  (:method ((class built-in-class))
    (find-inspector-component-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-inspector-component-type-for-compound-type type))

  (:method ((class structure-class))
    (find-inspector-component-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-inspector-component-type-for-prototype (class-prototype class))))

(def (function) find-inspector-component-type-for-compound-type (type)
  (find-inspector-component-type-for-compound-type* (first type) type))

(def (generic e) find-inspector-component-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-inspector-component-type-for-type (first main-type))
          (find-inspector-component-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          `(standard-object-list-component :the-class ,(find-type-by-name main-type))
          'list-component))))

(def (function e) make-inspector-component-for-prototype (prototype &rest args &key &allow-other-keys)
  (apply #'make-instance (find-inspector-component-type-for-prototype prototype) args))

(def (generic e) find-inspector-component-type-for-prototype (prototype)
  (:method ((prototype string))
    'string-component)

  (:method ((prototype symbol))
    'symbol-component)

  (:method ((prototype integer))
    'integer-component)

  (:method ((prototype float))
    'float-component)

  (:method ((prototype number))
    'number-component)

  (:method ((prototype list))
    'list-component)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition-component)

  (:method ((prototype structure-class))
    'standard-class-component)

  (:method ((prototype standard-class))
    'standard-class-component)

  (:method ((prototype structure-object))
    'standard-object-component)

  (:method ((prototype standard-object))
    'standard-object-component))

;;;;;;
;;; Viewer

(def (generic e) make-viewer-component (value &key &allow-other-keys)
  (:method (value &rest args &key type &allow-other-keys)
    (remove-from-plistf args :type)
    (prog1-bind component
        (apply #'make-inspector-component
               (or type
                   (if (and (typep value 'proper-list)
                            (every-type-p 'standard-object value))
                       '(list standard-object)
                       (class-of value)))
               args)
      (setf (component-value-of component) value))))

;;;;;;
;;; Editor

(def (generic e) make-editor-component (value &key &allow-other-keys)
  (:method (value &rest args &key &allow-other-keys)
    (prog1-bind component
        (apply #'make-viewer-component value args)
      (begin-editing component))))

;;;;;;
;;; Filter

(def (function e) make-filter-component (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-filter-component-type-for-type type))))
    (unless (subtypep component-type 'alternator-component)
      (remove-from-plistf args :default-component-type))
    (prog1-bind component (apply #'make-instance component-type (append args additional-args))
      (when (typep component 'editable-component)
        (begin-editing component)))))

(def (generic e) find-filter-component-type-for-type (type)
  (:method ((type symbol))
    (find-filter-component-type-for-type (find-type-by-name type)))

  (:method ((class built-in-class))
    (find-filter-component-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (class-prototype class)))))

  (:method ((type cons))
    (find-filter-component-type-for-compound-type type))

  (:method ((class structure-class))
    (find-filter-component-type-for-prototype (class-prototype class)))

  (:method ((class standard-class))
    (find-filter-component-type-for-prototype (class-prototype class))))

(def function find-filter-component-type-for-compound-type (type)
  (find-filter-component-type-for-compound-type* (first type) type))

(def (generic e) find-filter-component-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-filter-component-type-for-type (first main-type))
          (find-filter-component-type-for-type t)))))

(def (generic e) find-filter-component-type-for-prototype (prototype)
  (:method ((prototype string))
    'string-component)

  (:method ((prototype symbol))
    'symbol-component)

  (:method ((prototype integer))
    'integer-component)

  (:method ((prototype float))
    'float-component)

  (:method ((prototype number))
    'number-component)

  (:method ((prototype local-time:timestamp))
    'timestamp-component)

  (:method ((prototype structure-object))
    `(standard-object-filter-component :the-class ,(class-of prototype)))

  (:method ((prototype standard-object))
    `(standard-object-filter-component :the-class ,(class-of prototype))))

;;;;;;
;;; Maker

(def (function e) make-maker-component (type &rest args &key &allow-other-keys)
  (bind (((component-type &rest additional-args)
          (ensure-list (find-maker-component-type-for-type type))))
    (unless (subtypep component-type 'alternator-component)
      (remove-from-plistf args :default-component-type))
    (prog1-bind component
        (apply #'make-instance component-type (append args additional-args))
      (when (typep component 'editable-component)
        (begin-editing component)))))

(def (generic e) find-maker-component-type-for-type (type)
  (:method (type)
    (find-inspector-component-type-for-type type))

  (:method ((type symbol))
    (find-maker-component-type-for-type (find-type-by-name type)))

  (:method ((type cons))
    (find-maker-component-type-for-compound-type type))

  (:method ((class structure-class))
    `(standard-object-maker-component :the-class ,class))

  (:method ((class standard-class))
    `(standard-object-maker-component :the-class ,class)))

(def function find-maker-component-type-for-compound-type (type)
  (find-maker-component-type-for-compound-type* (first type) type))

(def (generic e) find-maker-component-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type (find-main-type-in-or-type type)))
      (if (= 1 (length main-type))
          (find-maker-component-type-for-type (first main-type))
          (find-maker-component-type-for-type t)))))

;;;;;;
;;; Utility

;; TODO: KLUDGE:
(def function find-main-type-in-or-type (type)
  (remove-if (lambda (element)
               (member element '(or null)))
             type))
