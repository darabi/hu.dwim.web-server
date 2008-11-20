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
    instance))

(def method refresh-component :before ((self abstract-standard-object-component))
  (bind ((instance (instance-of self))
         (reused-instance (reuse-standard-object-instance (class-of instance) instance)))
    (unless (eq instance reused-instance)
      (setf (instance-of self) reused-instance))))

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

(def component abstract-standard-object-tree-component (abstract-standard-object-component abstract-standard-class-component)
  ((parent-provider nil :type (or symbol function))
   (children-provider nil :type (or symbol function)))
  (:documentation "Base class with a TREE of STANDARD-OBJECT instances as component value."))

(def method clone-component ((self abstract-standard-object-tree-component))
  (prog1-bind clone (call-next-method)
    (setf (parent-provider-of clone) (parent-provider-of self))
    (setf (children-provider-of clone) (children-provider-of self))))

;;;;;;
;;; Abstract selectable standard object

(def component abstract-selectable-standard-object-component ()
  ((selected-instance-set (compute-as (or -current-value- (make-hash-table :test #'eql))) :type (or null hash-table))
   (minimum-selection-cardinality 0 :type fixnum)
   (maximum-selection-cardinality 1 :type fixnum)))

(def (generic e) single-selection-mode-p (component)
  (:method ((self abstract-selectable-standard-object-component))
    (= 1 (maximum-selection-cardinality-of self))))

(def (generic e) selected-instance-of (component)
  (:method ((self abstract-selectable-standard-object-component))
    (assert (single-selection-mode-p self))
    (first (selected-instances-of self))))

(def (generic e) selected-instances-of (component)
  (:method ((self abstract-selectable-standard-object-component))
    (awhen (selected-instance-set-of self)
      (hash-table-values it))))

(def (generic e) (setf selected-instances-of) (new-value component)
  (:method (new-value (self abstract-selectable-standard-object-component))
    (bind ((selected-instance-set (selected-instance-set-of self)))
      (clrhash selected-instance-set)
      (dolist (instance new-value)
        (setf (gethash (hash-key-for instance) selected-instance-set) instance))
      (invalidate-computed-slot self 'selected-instance-set))))

(def (generic e) selected-instance-p (component instance)
  (:method ((component abstract-selectable-standard-object-component) instance)
    (awhen (selected-instance-set-of component)
      (gethash (hash-key-for instance) it))))

(def (generic e) (setf selected-instance-p) (new-value component instance)
  (:method (new-value (component abstract-selectable-standard-object-component) instance)
    (bind ((selected-instance-set (selected-instance-set-of component)))
      (if new-value
          (setf (gethash (hash-key-for instance) selected-instance-set) instance)
          (remhash (hash-key-for instance) selected-instance-set))
      (invalidate-computed-slot component 'selected-instance-set))))

(def (function e) make-select-instance-command (component instance)
  (command (icon select)
           (make-action
             (with-restored-component-environment component
               (execute-select-instance component (class-of instance) instance)))))

(def (layered-function e) execute-select-instance (component class instance)
  (:method ((component abstract-selectable-standard-object-component) (class standard-class) (instance standard-object))
    (when (single-selection-mode-p component)
      (setf (selected-instances-of component) nil))
    (notf (selected-instance-p component instance))))

;;;;;
;;; Standard object detail component

(def component standard-object-detail-component (detail-component remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (slot-value-groups nil :type components)))

(def (layered-function e) collect-standard-object-detail-slot-groups (component class prototype slots)
  (:method ((component standard-object-detail-component) (class standard-class) (prototype standard-object) (slots list))
    (list (cons #"standard-object-detail-component.primary-group" slots))))

(defresources en
  (standard-object-detail-component.primary-group "Primary properties")
  (standard-object-detail-component.secondary-group "Other properties"))

(defresources hu
  (standard-object-detail-component.primary-group "Elsődleges tulajdonságok")
  (standard-object-detail-component.secondary-group "Egyéb tulajdonságok"))
;;;;;;
;;; Standard object slot value group 

(def component standard-object-slot-value-group-component (abstract-standard-slot-definition-group-component remote-identity-component-mixin)
  ((name nil :type component)
   (slot-values nil :type components)))

(def generic standard-object-slot-value-group-column-count (component)
  (:method ((self standard-object-slot-value-group-component))
    2))

(def render standard-object-slot-value-group-component ()
  (bind (((:read-only-slots name slot-values id) -self-))
    (if slot-values
        (progn
          <thead <tr <th (:colspan ,(standard-object-slot-value-group-column-count -self-)) ,(if name
                                                                                                 (render name)
                                                                                                 "NO")>>>
          <tbody ,(map nil #'render slot-values)>)
        <span (:id ,id) ,#"there-are-none">)))

(defresources en
  (standard-object-slot-value-group.there-are-no-slots "There are no properties"))

(defresources hu
  (standard-object-slot-value-group.there-are-no-slots "Nincs egy tulajdonság sem"))

;;;;;;
;;; Standard object slot value

(def component standard-object-slot-value-component (abstract-standard-slot-definition-component remote-identity-component-mixin)
  ((label nil :type component)
   (value nil :type component)))

(def render standard-object-slot-value-component ()
  (bind (((:read-only-slots label value id) -self-))
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
        <td (:class "slot-value-label")
            ,(render label)>
        <td (:class "slot-value-value")
            ,(render value)>>))
