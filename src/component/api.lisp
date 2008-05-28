;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; API

(def (generic e) make-component-for-type (type)
  (:method ((type symbol))
    (make-component-for-type (find-class type)))

  (:method ((type (eql 'boolean)))
    (make-instance 'boolean-component))

  (:method ((type cons))
    ;; KLUDGE: use perec type system
    (bind ((main-type
            (remove-if (lambda (element)
                         (member element '(or unbound null)))
                       type)))
      (if (= 1 (length main-type))
          (make-component-for-type (first main-type))
          (make-component-for-type t))))

  (:method ((class built-in-class))
    (make-component-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (closer-mop:class-prototype class)))))

  (:method ((class (eql (find-class t))))
    (make-instance 't-component))

  (:method ((class standard-class))
    (make-component-for-prototype (closer-mop:class-prototype class))))

(def (generic e) make-component-for-prototype (type)
  (:method ((prototype string))
    (make-instance 'string-component))

  (:method ((prototype integer))
    (make-instance 'integer-component))

  (:method ((prototype list))
    (make-instance 'list-component))

  (:method ((prototype standard-slot-definition))
    (make-instance 'standard-slot-definition-component))

  (:method ((prototype standard-class))
    (make-instance 'standard-class-component))

  (:method ((prototype standard-object))
    (make-instance 'standard-object-component))

  (:method ((prototype structure-object))
    (make-instance 'standard-object-component)))

(def (generic e) component-value-of (component))

(def (generic e) (setf component-value-of) (new-value component))

;;;;;;
;;; Viewer

(def (generic e) make-viewer-component (thing &key type &allow-other-keys)
  (:method (thing &key type &allow-other-keys)
    (aprog1 (make-component-for-type (or type (type-of thing)))
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

;; TODO:
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
