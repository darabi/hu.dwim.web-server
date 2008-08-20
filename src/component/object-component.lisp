;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard class

(def component abstract-standard-class-component ()
  ((the-class nil :type (or null standard-class)))
  (:documentation "Base class with a STANDARD-CLASS component value."))

(def method component-value-of ((component abstract-standard-class-component))
  (the-class-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-class-component))
  (setf (the-class-of component) new-value))

;;;;;;
;;; Abstract standard slot definition

(def component abstract-standard-slot-definition-component ()
  ((the-class nil :type (or null standard-class))
   (slot nil :type (or null standard-slot-definition)))
  (:documentation "Base class with a STANDARD-SLOT-DEFINITION component value."))

(def method component-value-of ((component abstract-standard-slot-definition-component))
  (slot-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-slot-definition-component))
  (setf (slot-of component) new-value))

;;;;;;
;;; Abstract standard slot definition group

(def component abstract-standard-slot-definition-group-component ()
  ((the-class nil :type (or null standard-class))
   (slots nil :type list))
  (:documentation "Base class with a LIST of STANDARD-SLOT-DEFINITION instances as component value."))

(def method component-value-of ((component abstract-standard-slot-definition-group-component))
  (slots-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-slot-definition-group-component))
  (setf (slots-of component) new-value))

;;;;;;
;;; Abstract standard object

(def component abstract-standard-object-component ()
  ((instance nil :type (or null standard-object)))
  (:documentation "Base class with a STANDARD-OBJECT component value."))

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

(def method refresh-component :before ((self abstract-standard-object-component))
  (bind ((instance (instance-of self)))
    (setf (instance-of self) (reuse-standard-object-instance (class-of instance) instance))))

;;;;;;
;;; Abstract standard object slot value

(def component abstract-standard-object-slot-value-component (abstract-standard-slot-definition-component abstract-standard-object-component)
  ()
  (:documentation "Base class with a STANDARD-SLOT-DEFINITION component value and a STANDARD-OBJECT instance."))

;;;;;;
;;; Abstract standard object list

(def component abstract-standard-object-list-component ()
  ((instances nil :type list))
  (:documentation "Base class with a LIST of STANDARD-OBJECT instances as component value."))

(def method component-value-of ((component abstract-standard-object-list-component))
  (instances-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-list-component))
  (setf (instances-of component) new-value))

(def method refresh-component :before ((self abstract-standard-object-list-component))
  ;; TODO: performance killer
  (setf (instances-of self)
        (mapcar (lambda (instance)
                  (reuse-standard-object-instance (class-of instance) instance))
                (instances-of self))))

;;;;;;
;;; Abstract standard object tree

(def component abstract-standard-object-tree-component (abstract-standard-object-component)
  ((parent-provider nil :type (or symbol function))
   (children-provider nil :type (or symbol function)))
  (:documentation "Base class with a TREE of STANDARD-OBJECT instances as component value."))

(def method clone-component ((self abstract-standard-object-tree-component))
  (prog1-bind clone (call-next-method)
    (setf (parent-provider-of clone) (parent-provider-of self))
    (setf (children-provider-of clone) (children-provider-of self))))

;;;;;;
;;; Standard object slot value group 

(def component standard-object-slot-value-group-component (abstract-standard-slot-definition-group-component remote-identity-component-mixin)
  ((slot-values nil :type components)))

(def render standard-object-slot-value-group-component ()
  (with-slots (slot-values id) -self-
    (if slot-values
        <table (:id ,id :class "slot-value-group")
          <thead <tr <th ,#"standard-object-slot-value-group.column.name">
                     <th ,#"standard-object-slot-value-group.column.value">>>
          <tbody ,(map nil #'render slot-values)>>
        <span (:id ,id) ,#"there-are-none">)))

(defresources en
  (standard-object-slot-value-group.there-are-no-slots "There are no slots")
  (standard-object-slot-value-group.column.name "Name")
  (standard-object-slot-value-group.column.value "Value"))

(defresources hu
  (standard-object-slot-value-group.there-are-no-slots "Nincs egy tulajdonság sem")
  (standard-object-slot-value-group.column.name "Név")
  (standard-object-slot-value-group.column.value "Érték"))

;;;;;;
;;; Standard object slot value

(def component standard-object-slot-value-component (abstract-standard-slot-definition-component remote-identity-component-mixin)
  ((label nil :type component)
   (value nil :type component)))

(def render standard-object-slot-value-component ()
  (with-slots (label value id) -self-
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
        <td (:class "slot-value-label")
            ,(render label)>
        <td (:class "slot-value-value")
            ,(render value)>>))
