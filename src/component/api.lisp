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
    (make-component-for-type (first (remove-if (lambda (element)
                                                 (member element '(or unbound null)))
                                               type))))

  (:method ((class built-in-class))
    (make-component-for-prototype
     (case (class-name class)
       (string "42")
       (list nil)
       (t (closer-mop:class-prototype class)))))

  (:method ((class standard-class))
    (make-component-for-prototype (closer-mop:class-prototype class))))

(def (generic e) make-component-for-prototype (type)
  (:method ((prototype string))
    (make-instance 'string-component))

  (:method ((prototype integer))
    (make-instance 'integer-component))

  (:method ((prototype list))
    (error "TODO:"))

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
