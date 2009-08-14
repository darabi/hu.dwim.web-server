;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization points

;; TODO: why not (make-inspector 'standard-class) ?
(def (layered-function e) make-standard-class-presentation (component class prototype)
  (:documentation "Creates a component that can present a STANDARD-CLASS instance.")

  (:method ((component component) (class standard-class) (prototype standard-object))
    (localized-class-name class)))

;;;;;;
;;; Standard object mixin

(def (component e) standard-object/mixin ()
  ((instance
    nil
    :type (or null standard-object)
    :computed-in compute-as))
  (:documentation "A component with a STANDARD-OBJECT component value."))

(def method call-compute-as ((self standard-object/mixin) thunk)
  (aprog1 (call-next-method)
    (when (typep it 'standard-object)
      (reuse-standard-object-instance (class-of it) it))))

(def method component-value-of ((component standard-object/mixin))
  (instance-of component))

(def method (setf component-value-of) (new-value (component standard-object/mixin))
  (setf (instance-of component) new-value))

(def method component-dispatch-class ((self standard-object/mixin))
  (awhen (instance-of self)
    (class-of it)))

(def (layered-function e) reuse-standard-object-instance (class instance)
  (:method ((class built-in-class) (instance null))
    nil)

  (:method ((class standard-class) (instance standard-object))
    instance))

(def layered-method refresh-component :before ((self standard-object/mixin))
  (bind ((instance (instance-of self))
         (reused-instance (reuse-standard-object-instance (class-of instance) instance)))
    (unless (eq instance reused-instance)
      (setf (instance-of self) reused-instance))))

;;;;;;
;;; Abstract standard object slot value component

(def (component e) standard-object-slot/mixin (standard-slot-definition/mixin standard-object/mixin)
  ()
  (:documentation "A component with a STANDARD-SLOT-DEFINITION component value and a STANDARD-OBJECT instance."))

;;;;;;
;;; Standard object list mixin

(def (component e) standard-object-list/mixin ()
  ((instances nil :type list))
  (:documentation "A component with a LIST of STANDARD-OBJECT instances as component value."))

(def method component-value-of ((component standard-object-list/mixin))
  (instances-of component))

(def method (setf component-value-of) (new-value (component standard-object-list/mixin))
  (setf (instances-of component) new-value))

(def layered-method refresh-component :before ((self standard-object-list/mixin))
  ;; TODO: performance killer
  (setf (instances-of self)
        (mapcar (lambda (instance)
                  (reuse-standard-object-instance (class-of instance) instance))
                (instances-of self))))

;;;;;;
;;; Standard object tree mixin

(def (component e) standard-object-tree/mixin (standard-object-list/mixin standard-class/mixin)
  ((parent-provider nil :type (or symbol function))
   (children-provider nil :type (or symbol function)))
  (:documentation "A component with a TREE of STANDARD-OBJECT instances as component value."))

(def constructor (standard-object-tree/mixin (instance nil instance?))
  (when instance?
    (setf (instances-of -self-) (list instance))))

(def method clone-component ((self standard-object-tree/mixin))
  (prog1-bind clone (call-next-method)
    (setf (parent-provider-of clone) (parent-provider-of self))
    (setf (children-provider-of clone) (children-provider-of self))))

;;;;;;
;;; Abstract standard object node component

;; TODO: tree, tree-node and table, table-row or tree, node and table, row
(def (component e) abstract-standard-object-node-component (standard-object/mixin standard-class/mixin)
  ((parent-provider nil :type (or symbol function))
   (children-provider nil :type (or symbol function)))
  (:documentation "A component with a TREE of STANDARD-OBJECT instances as component value."))

;;;;;;
;;; Abstract selectable standard object component

(def (component e) abstract-selectable-standard-object-component ()
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

(def (layered-function e) make-select-instance-command (component class prototype instance)
  (:method ((component component) (class standard-class) (prototype standard-object) (instance standard-object))
    (command ()
      (icon select)
      (make-component-action component
        (execute-select-instance component (class-of instance) instance)))))

(def (layered-function e) execute-select-instance (component class instance)
  (:method ((component component) (class standard-class) (instance standard-object))
    ;; TODO delme eventually (attila)
    (execute-select-instance (find-ancestor-component-with-type component 'abstract-selectable-standard-object-component) class instance))

  (:method ((component abstract-selectable-standard-object-component) (class standard-class) (instance standard-object))
    (when (single-selection-mode-p component)
      (setf (selected-instances-of component) nil))
    (notf (selected-instance-p component instance))))

;;;;;
;;; Standard object detail component

(def (component e) standard-object-detail-component (detail-component id/mixin)
  ((slot-value-groups nil :type components)))

(def (layered-function e) collect-standard-object-detail-slot-groups (component class prototype slots)
  (:method ((component standard-object-detail-component) (class standard-class) (prototype standard-object) (slots list))
    (list (cons #"standard-object-detail-component.primary-group" slots))))

(def render-component standard-object-detail-component
  (foreach #'render-component (slot-value-groups-of -self-)))

(def function find-slot-value-group-component (slot-group slot-value-groups)
  (find slot-group slot-value-groups :key #'slots-of
        :test (lambda (slot-group-1 slot-group-2)
                (every (lambda (slot-1 slot-2)
                         (eq (slot-definition-name slot-1)
                             (slot-definition-name slot-2)))
                       slot-group-1
                       slot-group-2))))

;;;;;;
;;; Standard object slot value group component

(def (component e) standard-object-slot-value-group-component (standard-slot-definition-list/mixin id/mixin)
  ((name nil :type component)
   (slot-values nil :type components)))

(def generic standard-object-slot-value-group-column-count (component)
  (:method ((self standard-object-slot-value-group-component))
    2))

(def render-xhtml standard-object-slot-value-group-component
  (bind (((:read-only-slots name slot-values id) -self-))
    (if slot-values
        (progn
          (when name
            <thead <tr <th (:class "slot-value-group" :colspan ,(standard-object-slot-value-group-column-count -self-))
                           <div (:id ,id) ,(render-component name)>>>>
            (render-remote-setup -self-))
          <tbody ,(foreach #'render-component slot-values)>)
        <span (:id ,id) ,#"there-are-none">)))

(def render-component standard-object-slot-value-group-component
  (foreach #'render-component (slot-values-of -self-)))

(def function find-slot-value-component (slot slot-values)
  (find (slot-definition-name slot) slot-values
        :key (lambda (slot-value)
               (slot-definition-name (slot-of slot-value)))))

;;;;;;
;;; Standard object slot value inspector

(def (component e) standard-object-slot-value/inspector (standard-object-slot/mixin component-messages/basic id/mixin)
  ((label nil :type component)
   (value nil :type component)))

(def render-xhtml standard-object-slot-value/inspector
  (bind (((:read-only-slots label value id messages) -self-))
    (when messages
      <tr <td (:colspan 2) ,(render-component-messages -self-)>>)
    <tr (:id ,id :class ,(element-style-class -self- (slot-values-of (parent-component-of -self-))))
        <td (:class "slot-value-label") ,(render-component label)>
        <td (:class "slot-value-value") ,(render-component value)>>))

(def render-csv standard-object-slot-value/inspector
  (render-component (label-of -self-))
  (write-csv-value-separator)
  (render-component (value-of -self-))
  (write-csv-line-separator))

;;;;;;
;;; Standard class selector

(def (component e) standard-class/selector (member/inspector)
  ()
  (:default-initargs :edited #t :client-name-generator [localized-class-name !2]))

(def constructor standard-class/selector ()
  (setf (the-type-of -self-) `(or null (member ,@(possible-values-of -self-)))))

(def function make-class-selector (classes)
  (make-instance 'standard-class/selector :component-value (first classes) :possible-values classes))

(def render-xhtml standard-class/selector
  (if (edited? -self-)
      (bind ((href (register-action/href (make-action (mark-to-be-refreshed-component (parent-component-of -self-))))))
        (render-member-component -self- :on-change `js-inline(wui.io.action ,href :ajax #f)))
      (call-next-method)))

;;;;;;
;;; Standard slot definition selector

(def (component e) standard-slot-definition/selector (member/inspector)
  ()
  (:default-initargs :edited #t :client-name-generator [if !2
                                                           (localized-slot-name !2)
                                                           ""]))

(def function make-slot-selector (slots)
  (make-instance 'standard-slot-definition/selector :component-value nil :possible-values (list* nil slots)))
