;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (function e) make-component-for-type (type)
  (make-instance (find-component-type-for-type type)))

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

(def (function e) make-component-for-prototype (type)
  (make-instance (find-component-type-for-prototype type)))

(def (generic e) find-component-type-for-prototype (type)
  (:method ((prototype string))
    'string-component)

  (:method ((prototype integer))
    'integer-component)

  (:method ((prototype list))
    'list-component)

  (:method ((prototype standard-slot-definition))
    'standard-slot-definition-component)

  (:method ((prototype standard-class))
    'standard-class-component)

  (:method ((prototype standard-object))
    'standard-object-component)

  (:method ((prototype structure-object))
    'standard-object-component))

(def (generic e) component-value-of (component))

(def (generic e) (setf component-value-of) (new-value component))

;;;;;;
;;; Viewer

(def (generic e) make-viewer-component (thing &key type &allow-other-keys)
  (:method (thing &key type &allow-other-keys)
    (aprog1 (make-component-for-type
             (or type
                 (if (and (typep thing 'proper-list)
                          (every-type-p 'standard-object thing))
                     '(list standard-object)
                     (type-of thing))))
      (setf (component-value-of it) thing))))

;;;;;;
;;; Editor

(def (generic e) make-editor-component (thing &key type &allow-other-keys)
  (:method (thing &key type &allow-other-keys)
    (aprog1 (make-component-for-type (or type (type-of thing)))
      (setf (component-value-of it) thing)
      (begin-editing it))))

;;;;;;
;;; Filter

;; TODO: join with make-component-for-type
(def (generic e) make-filter-component (thing)
  (:method ((class-name (eql t)))
    ;; KLUDGE: take a filter form as parameter and use that
    (make-filter-component (find-class 'standard-object)))

  (:method ((class-name symbol))
    (make-filter-component (find-class class-name)))

  (:method ((class-name (eql 'boolean)))
    (make-instance 'boolean-component :edited #t))

  (:method ((type cons))
    ;; KLUDGE:
    (make-filter-component (first (remove 'null (remove 'or type)))))

  (:method ((class built-in-class))
    (make-instance 'string-component :edited #t))

  (:method ((class (eql (find-class 'string))))
    (make-instance 'string-component :edited #t))

  (:method ((class (eql (find-class 'integer))))
    (make-instance 'integer-component :edited #t))

  (:method ((class standard-class))
    (make-instance 'standard-object-filter-component :the-class class)))

;;;;;;
;;; Maker

;; TODO:
(def (generic e) make-maker-component (thing)
  (:method ((class standard-class))
    (aprog1 (make-instance 'standard-object-maker-component :the-class class)
      (begin-editing it))))
