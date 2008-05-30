;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (function e) make-component-for-type (type &rest args &key &allow-other-keys)
  (apply #'make-instance (find-component-type-for-type type) args))

(def (generic e) find-component-type-for-type (type)
  (:method ((type symbol))
    (find-component-type-for-type (find-class type)))

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
    (find-component-type-for-prototype (closer-mop:class-prototype class))))

(def (function) find-component-type-for-compound-type (type)
  (find-component-type-for-compound-type* (first type) type))

;; KLUDGE: use perec type system
(def (generic e) find-component-type-for-compound-type* (first type)
  (:method ((first (eql 'or)) (type cons))
    (bind ((main-type
            (remove-if (lambda (element)
                         (member element '(or unbound null)))
                       type)))
      (if (= 1 (length main-type))
          (find-component-type-for-type (first main-type))
          (find-component-type-for-type t))))

  (:method ((first (eql 'list)) (type cons))
    (bind ((main-type (second type)))
      (if (subtypep main-type 'standard-object)
          'standard-object-list-component
          'list-component))))

(def (function e) make-component-for-prototype (type &rest args &key &allow-other-keys)
  (apply #'make-instance (find-component-type-for-prototype type) args))

(def (generic e) find-component-type-for-prototype (type)
  (:method ((prototype string))
    'string-component)

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

;; TODO: join with make-component-for-type
(def (generic e) make-filter-component (thing &key &allow-other-keys)
  (:method ((class-name (eql t)) &rest args &key &allow-other-keys)
    ;; KLUDGE: take a filter form as parameter and use that
    (apply #'make-filter-component (find-class 'standard-object) args))

  (:method ((class-name (eql 'dmm::standard-text)) &key &allow-other-keys)
    (make-instance 'string-component))

  (:method ((class-name (eql 'prc::timestamp)) &key &allow-other-keys)
    (make-instance 'timestamp-component))

  (:method ((class-name (eql 'prc::date)) &key &allow-other-keys)
    (make-instance 'date-component))

  (:method ((class-name (eql 'prc::time)) &key &allow-other-keys)
    (make-instance 'time-component))

  (:method ((class-name (eql 'prc::ip-address)) &key &allow-other-keys)
    (make-instance 'time-component))

  (:method ((class-name symbol) &rest args &key &allow-other-keys)
    (apply #'make-filter-component (find-class class-name) args))

  (:method ((class-name (eql 'boolean)) &key &allow-other-keys)
    (make-instance 'boolean-component :edited #t))

  (:method ((type cons) &rest args &key &allow-other-keys)
    (apply #'make-filter-component-for-compound-type type args))

  (:method ((class built-in-class) &key &allow-other-keys)
    (make-instance 'string-component :edited #t))

  (:method ((class (eql (find-class 'string))) &key &allow-other-keys)
    (make-instance 'string-component :edited #t))

  (:method ((class (eql (find-class 'symbol))) &key &allow-other-keys)
    (make-instance 'symbol-component :edited #t))

  (:method ((class (eql (find-class 'fixnum))) &key &allow-other-keys)
    (make-instance 'integer-component :edited #t))

  (:method ((class (eql (find-class 'integer))) &key &allow-other-keys)
    (make-instance 'integer-component :edited #t))

  (:method ((class structure-class) &key default-component-type &allow-other-keys)
    (make-instance 'standard-object-filter-component :the-class class :default-component-type default-component-type))

  (:method ((class standard-class) &key default-component-type &allow-other-keys)
    (make-instance 'standard-object-filter-component :the-class class :default-component-type default-component-type)))

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
(def (generic e) make-maker-component (thing)
  (:method ((class standard-class))
    (aprog1 (make-instance 'standard-object-maker-component :the-class class)
      (begin-editing it))))
