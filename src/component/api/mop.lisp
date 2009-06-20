;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; MOP

(def (class* e) component-class (computed-class)
  ((computed-slots nil :type list)
   (component-slots nil :type list))
  (:documentation "Default class meta object for components."))

(def class component-slot-definition (standard-slot-definition)
  ()
  (:documentation "Default slot meta object base class for components."))

(def class component-direct-slot-definition (component-slot-definition standard-direct-slot-definition)
  ()
  (:documentation "Default direct slot meta object for components."))

(def class component-effective-slot-definition (component-slot-definition standard-effective-slot-definition)
  ()
  (:documentation "Default effective slot meta object for components."))

(def method validate-superclass ((class component-class) (superclass standard-class))
  #t)

(def function component-slot? (args)
  (bind ((child-component (getf args :child-component #t))
         (type (getf args :type #t)))
    (and child-component
         (or (subtypep type 'component)
             (subtypep type '(or null component))
             (or (subtypep type 'components)
                 (and (consp type)
                      (eq (first type) 'components)
                      (subtypep (second type) 'component)))))))

(def method direct-slot-definition-class ((class component-class) &rest args)
  (if (component-slot? args)
      (find-class 'component-direct-slot-definition)
      (call-next-method)))

(def method effective-slot-definition-class ((class component-class) &rest args)
  (if (component-slot? args)
      (find-class 'component-effective-slot-definition)
      (call-next-method)))

;;; Ensure standard-component is among the supers of the instances of component-class
(def function shared-initialize-around-component-class (class class-name direct-superclasses next-method initargs)
  (declare (dynamic-extent initargs))
  (if (or (eq class-name 'component)
          (loop :for class :in direct-superclasses
                :thereis (ignore-errors
                           (subtypep class (find-class 'component)))))
      (funcall next-method)
      (apply next-method class
             :direct-superclasses (append direct-superclasses
                                          (unless (eq (getf initargs :name) 'component)
                                            (list (find-class 'component))))
             initargs)))

(def method initialize-instance :around ((class component-class) &rest initargs &key name direct-superclasses)
  (declare (dynamic-extent initargs))
  (shared-initialize-around-component-class class name direct-superclasses #'call-next-method initargs))

(def method reinitialize-instance :around ((class component-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (shared-initialize-around-component-class class (class-name class) direct-superclasses #'call-next-method initargs)
      ;; if direct superclasses are not explicitly passed we _must_ not change anything
      (call-next-method)))

(def method finalize-inheritance :after ((class component-class))
  (bind ((slots (class-slots class)))
    (setf (computed-slots-of class) (filter-if (of-type 'computed-effective-slot-definition) slots)
          (component-slots-of class) (filter-if (of-type 'component-effective-slot-definition) slots))))
