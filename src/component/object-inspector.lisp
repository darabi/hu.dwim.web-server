;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector

(def component inspector-component ()
  ())

;;;;;;
;;; Standard object

(def component standard-object-inspector (abstract-standard-object-component
                                          inspector-component
                                          editable-component
                                          alternator-component
                                          user-message-collector-component-mixin
                                          remote-identity-component-mixin
                                          initargs-component-mixin
                                          layered-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-inspector-alternatives)
  (:documentation "Inspector for an instance of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object-inspector (instance)
  `(make-instance 'standard-object-inspector :instance ,instance))

(def method refresh-component ((self standard-object-inspector))
  (with-slots (instance default-component-type alternatives content command-bar) self
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) instance)
              (setf alternatives (funcall (alternatives-factory-of self) self (class-of instance) instance)))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) instance)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-alternator-command-bar self alternatives
                                                         (append (list (make-open-in-new-frame-command self)
                                                                       (make-top-command self)
                                                                       (make-refresh-command self))
                                                                 (make-standard-object-inspector-commands self (class-of instance) instance)))))
        (setf alternatives (list (delay-alternative-component-with-initargs 'null-component))
              content (find-default-alternative-component alternatives)))))

(def render standard-object-inspector ()
  (with-slots (id) -self-
    <div (:id ,id :class "standard-object")
         ,(render-user-messages -self-)
         ,(call-next-method)>))

(def (layered-function e) make-standard-object-inspector-alternatives (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-inspector :instance instance)
          (delay-alternative-reference-component 'standard-object-reference instance))))

(def (layered-function e) make-standard-object-inspector-commands (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (list (make-editing-commands component)
          (make-new-instance-command component)))

  (:method ((component standard-object-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
              (make-editing-commands component))
            (optional-list (when (dmm::authorize-operation 'dmm::create-entity-operation :-entity- class)
                             (make-new-instance-command component))
                           (when (dmm::authorize-operation 'dmm::delete-entity-operation :-entity- class)
                             (make-delete-instance-command component))))))

(def (function e) make-new-instance-command (component)
  (make-instance 'command-component
                 :icon (icon new)
                 :visible (delay (not (edited-p component)))
                 :action (make-action
                           (not-yet-implemented))))

(def (function e) make-delete-instance-command (self)
  (make-instance 'command-component
                 :icon (icon delete)
                 :visible (delay (not (edited-p self)))
                 :action (make-action
                           (bind ((instance (instance-of self)))
                             (execute-delete-instance self (class-of instance) instance)))))

(def (generic e) execute-delete-instance (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (prc::purge-instance (instance-of component))
    (setf (component-value-of component) nil))

  (:method ((component standard-object-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (rdbms::with-transaction
      (call-next-method))))

;;;;;;
;;; Standard object detail

(def component standard-object-detail-inspector (abstract-standard-object-component
                                                 inspector-component
                                                 editable-component
                                                 detail-component
                                                 remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (slot-value-groups nil :type components))
  (:documentation "Inspector for an instance of STANDARD-OBJECT in detail"))

(def method refresh-component ((self standard-object-detail-inspector))
  (with-slots (instance class slot-value-groups) self
    (bind ((the-class (when instance (class-of instance))))
      (if the-class
          (if class
              (setf (component-value-of class) the-class)
              (setf class (make-viewer-component the-class :default-component-type 'reference-component)))
          (setf class nil))
      (if instance
          (bind ((slots (collect-standard-object-detail-inspector-slots self the-class instance))
                 (slot-groups (collect-standard-object-detail-inspector-slot-value-groups self the-class instance slots)))
            (setf slot-value-groups
                  (iter (for slot-group :in slot-groups)
                        (when slot-group
                          (for slot-value-group = (find slot-group slot-value-groups :key 'slots-of :test 'equal))
                          (if slot-value-group
                              (setf (component-value-of slot-value-group) slot-group
                                    (instance-of slot-value-group) instance
                                    (the-class-of slot-value-group) the-class)
                              (setf slot-value-group (make-instance 'standard-object-slot-value-group-inspector :instance instance :slots slot-group)))
                          (collect slot-value-group)))))
          (setf slot-value-groups nil)))))

(def (generic e) collect-standard-object-detail-inspector-slot-value-groups (component class instance slots)
  (:method ((component standard-object-detail-inspector) (class standard-class) (instance standard-object) (slots list))
    slots)

  (:method ((component standard-object-detail-inspector) (class dmm::entity) (instance prc::persistent-object) (slots list))
    (partition slots #'dmm::primary-p (constantly #t))))

(def (generic e) collect-standard-object-detail-inspector-slots (component class instance)
  (:method ((component standard-object-detail-inspector) (class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((component standard-object-detail-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-detail-inspector) (class dmm::entity) (instance prc::persistent-object))
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(def render standard-object-detail-inspector ()
  (with-slots (class slot-value-groups id) -self-
    <div (:id ,id :class "standard-object")
         <span ,#"standard-object-detail-inspector.instance" " " ,(render class)>
         <div <h3 ,#"standard-object-detail-inspector.slots">
              ,(map nil #'render slot-value-groups)>>))

(defresources en
  (standard-object-detail-inspector.instance "An instance of")
  (standard-object-detail-inspector.slots "Slots"))

(defresources hu
  (standard-object-detail-inspector.instance "Egy")
  (standard-object-detail-inspector.slots "Tulajdonságok"))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-inspector (standard-object-slot-value-group-component
                                                           abstract-standard-object-component
                                                           inspector-component
                                                           editable-component)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITIONs"))

(def method refresh-component ((self standard-object-slot-value-group-inspector))
  (with-slots (instance slots slot-values) self
    (if instance
        (setf slot-values
              (iter (for slot :in slots)
                    (for slot-value-component = (find slot slot-values :key #'component-value-of))
                    (if slot-value-component
                        (setf (component-value-of slot-value-component) slot
                              (instance-of slot-value-component) instance)
                        (setf slot-value-component (make-standard-object-slot-value-inspector self (class-of instance) instance slot)))
                    (collect slot-value-component)))
        (setf slot-values nil))))

(def (generic e) make-standard-object-slot-value-inspector (component class instance slot)
  (:method ((component standard-object-slot-value-group-inspector) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-inspector :instance instance :slot slot)))

;;;;;;
;;; Standard object slot value detail

(def component standard-object-slot-value-inspector (standard-object-slot-value-component
                                                     abstract-standard-object-component
                                                     inspector-component
                                                     editable-component)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION"))

(def method refresh-component ((self standard-object-slot-value-inspector))
  (with-slots (instance slot label value) self
    (if slot
        (if label
            (setf (component-value-of label) (localized-slot-name slot))
            (setf label (label (localized-slot-name slot))))
        (setf label nil))
    (if instance
        (if value
            (setf (place-of value) (make-slot-value-place instance slot))
            (setf value (make-instance 'place-inspector :place (make-slot-value-place instance slot))))
        (setf value nil))))
