;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object

(def component abstract-standard-object-component ()
  ((instance nil :type (or null standard-object)))
  (:documentation "Base class with a STANDARD-OBJECT component value"))

(def method component-value-of ((component abstract-standard-object-component))
  (instance-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-component))
  (setf (instance-of component) new-value))

(def (generic e) reuse-standard-object-instance (class instance)
  (:method ((class built-in-class) (instance null))
    nil)

  (:method ((class standard-class) (instance standard-object))
    instance)

  (:method ((class prc::persistent-class) (instance prc::persistent-object))
    (if (prc::persistent-p instance)
        (prc::load-instance instance)
        (call-next-method))))

(def function reuse-abstract-standard-object-component (self)
  (bind ((instance (instance-of self)))
    (setf (instance-of self) (reuse-standard-object-instance (class-of instance) instance))))

(def render :before abstract-standard-object-component
  (reuse-abstract-standard-object-component -self-))

(def method refresh-component :before ((self abstract-standard-object-component))
  (reuse-abstract-standard-object-component self))

;;;;;;
;;; Abstract standard object slot value

(def component abstract-standard-object-slot-value-component (abstract-standard-slot-definition-component abstract-standard-object-component)
  ())

;;;;;;
;;; Standard object list

(def component abstract-standard-object-list-component ()
  ((instances nil :type list)))

(def method component-value-of ((component abstract-standard-object-list-component))
  (instances-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-list-component))
  (setf (instances-of component) new-value))

(def function reuse-abstract-standard-object-list-component (self)
  ;; TODO: performance killer
  (setf (instances-of self)
        (mapcar (lambda (instance)
                  (reuse-standard-object-instance (class-of instance) instance))
                (instances-of self))))

(def render :before abstract-standard-object-list-component ()
  (reuse-abstract-standard-object-list-component -self-))

(def method refresh-component :before ((self abstract-standard-object-list-component))
  (reuse-abstract-standard-object-list-component self))

;;;;;;
;;; Standard object tree

(def component abstract-standard-object-tree-component (abstract-standard-object-component)
  ((children-provider nil :type (or symbol function)))
  (:documentation "Base class for a tree of STANDARD-OBJECT component value"))

(def method clone-component ((self abstract-standard-object-tree-component))
  (prog1-bind clone (call-next-method)
    (setf (children-provider-of clone) (children-provider-of self))))
