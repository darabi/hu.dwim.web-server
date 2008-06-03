;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (function e) make-component-for-type (type &rest args &key &allow-other-keys)
  "A TYPE specifier is either
     - a primitive type name such as boolean, integer, string
     - a parameterized type specifier such as (integer 100 200) 
     - a compound type specifier such as (or null string)
     - a type alieas name refering to a parameterized or compound type
     - a CLOS class name such as standard-object or audited-object
     - a CLOS type instance parse from a compound type specifier such as #<INTEGER-TYPE 0x1232112>"
  (apply #'make-instance (find-component-type-for-type type) args))

(def (generic e) find-component-type-for-type (type)
  (:method ((type symbol))
    (find-component-type-for-type (or (find-class type nil)
                                      (find-class (prc::type-class-name-for type)))))

  (:method ((type (eql 'boolean)))
    'boolean-component)

  (:method ((type cons))
    (find-component-type-for-compound-type type))

  (:method ((class built-in-class))
    (find-component-type-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (closer-mop:class-prototype class)))))

  (:method ((class (eql (find-class t))))
    't-component)

  (:method ((class structure-class))
    (find-component-type-for-prototype (closer-mop:class-prototype class)))

  (:method ((class standard-class))
    (find-component-type-for-prototype (closer-mop:class-prototype class)))

  (:method ((class prc::persistent-class))
    'standard-object-component))

(def (function) find-component-type-for-compound-type (type)
  (find-component-type-for-compound-type* (first type) type))

;; KLUDGE: use perec type system
(def (generic e) find-component-type-for-compound-type* (first type)
  (:method ((first (eql 'member)) (type cons))
    'member-component)

  (:method ((first (eql 'prc::text)) (type cons))
    'string-component)

  (:method ((first (eql 'prc::integer)) (type cons))
    'integer-component)

  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type
            (remove-if (lambda (element)
                         (member element '(or prc::unbound null)))
                       type)))
      (if (= 1 (length main-type))
          (find-component-type-for-type (first main-type))
          (find-component-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          'standard-object-list-component
          'list-component)))

  (:method ((first (eql 'prc::set)) (type cons))
    'standard-object-list-component))

(def (function e) make-component-for-prototype (type &rest args &key &allow-other-keys)
  (apply #'make-instance (find-component-type-for-prototype type) args))

(def (generic e) find-component-type-for-prototype (type)
  (:method ((prototype string))
    'string-component)

  (:method ((prototype symbol))
    'symbol-component)

  (:method ((prototype prc::string-type))
    'string-component)

  (:method ((prototype prc::integer-type))
    'integer-component)

  (:method ((prototype integer))
    'integer-component)

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

(def (generic e) component-value-of (component))

(def (generic e) (setf component-value-of) (new-value component))

;;;;;;
;;; Viewer

(def (generic e) make-viewer-component (thing &key &allow-other-keys)
  (:method (thing &rest args &key type &allow-other-keys)
    (remove-from-plistf args :type)
    (aprog1 (apply #'make-component-for-type
                   (or type
                       (if (and (typep thing 'proper-list)
                                (every-type-p 'standard-object thing))
                           '(list standard-object)
                           (type-of thing)))
                   args)
      (setf (component-value-of it) thing))))

;;;;;;
;;; Editor

(def (generic e) make-editor-component (thing &key &allow-other-keys)
  (:method (thing &rest args &key &allow-other-keys)
    (aprog1 (apply #'make-viewer-component thing args)
      (begin-editing it))))

;;;;;;
;;; Filter

(def (generic e) make-filter-component (thing &key &allow-other-keys)
  (:method (type &key &allow-other-keys)
    (prog1-bind component (make-instance (find-component-type-for-type type))
      (begin-editing component)))

  (:method ((type symbol) &rest args &key &allow-other-keys)
    (apply #'make-filter-component (or (find-class type nil)
                                       (find-class (prc::type-class-name-for type))) args))

  (:method ((type cons) &rest args &key &allow-other-keys)
    (apply #'make-filter-component-for-compound-type type args))

  (:method ((class structure-class) &rest args &key &allow-other-keys)
    (apply #'make-instance 'standard-object-filter-component :the-class class args))

  (:method ((class standard-class) &rest args &key &allow-other-keys)
    (apply #'make-instance 'standard-object-filter-component :the-class class args)))

(def function make-filter-component-for-compound-type (type &rest args &key &allow-other-keys)
  (apply #'make-filter-component-for-compound-type* (first type) type args))

(def (generic e) make-filter-component-for-compound-type* (first type &key &allow-other-keys)
  (:method ((first (eql 'prc::set)) (type cons) &key &allow-other-keys)
    ;; TODO:
    (make-instance 'null-component))

  (:method ((first (eql 'or)) (type cons) &rest args &key &allow-other-keys)
    ;; KLUDGE:
    (prog1-bind component (apply #'make-filter-component (last-elt type) args)
      (if (and (typep component 'atomic-component)
               (member 'null type))
          (setf (allow-nil-value-p component) #t))))

  (:method ((first (eql 'member)) (type cons) &key &allow-other-keys)
    ;; TODO:
    (make-instance 'member-component))

  (:method ((first (eql 'prc::text)) (type cons) &key &allow-other-keys)
    ;; TODO:
    (make-instance 'string-component)))

;;;;;;
;;; Maker

;; TODO:
(def (generic e) make-maker-component (thing &key &allow-other-keys)
  (:method (type &key &allow-other-keys)
    (prog1-bind component (make-instance (find-component-type-for-type type))
      (begin-editing component)))

  (:method ((class standard-class) &rest args &key &allow-other-keys)
    ;; TODO: persistent #f is sufficient?
    (aprog1 (apply #'make-instance 'standard-object-maker-component :the-class class args)
      (begin-editing it))))
