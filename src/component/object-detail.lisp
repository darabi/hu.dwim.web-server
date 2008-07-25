;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard object

(def component abstract-standard-object-component (value-component)
  ((instance nil :type (or null standard-object))
   (the-class nil :type standard-class))
  (:documentation "Base class with a STANDARD-OBJECT component value"))

(def method component-value-of ((component abstract-standard-object-component))
  (instance-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-component))
  (with-slots (instance the-class) component
    (setf instance new-value)
    (setf the-class (when new-value (class-of new-value)))))

(def generic reuse-standard-object-instance (instance)
  (:method (instance)
    instance)

  (:method ((instance prc::persistent-object))
    (if (prc::persistent-p instance)
        (prc::load-instance instance)
        (call-next-method))))

(def render :before abstract-standard-object-component
  (with-slots (instance) -self-
    (setf instance (reuse-standard-object-instance instance))))

(def method refresh-component :before ((self abstract-standard-object-component))
  (with-slots (instance) self
    (setf instance (reuse-standard-object-instance instance))))

;;;;;;
;;; Standard object

(def component standard-object-component (abstract-standard-object-component alternator-component editable-component
                                          user-message-collector-component-mixin remote-identity-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-alternatives)
  (:documentation "Component for an instance of STANDARD-OBJECT in various alternative views"))

(def method (setf component-value-of) :after (new-value (self standard-object-component))
  (with-slots (instance the-class default-component-type alternatives content command-bar) self
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) instance)
              (setf alternatives (funcall (alternatives-factory-of self) instance)))
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
                                                                 (make-standard-object-commands self the-class instance)))))
        (setf alternatives (list (delay-alternative-component-type 'null-component))
              content (find-default-alternative-component alternatives)))))

(def render standard-object-component ()
  (with-slots (id) -self-
    <div (:id ,id :class "standard-object")
         ,(render-user-messages -self-)
         ,(call-next-method)>))

(def (generic e) make-standard-object-alternatives (instance)
  (:method ((instance standard-object))
    (list (delay-alternative-component-type 'standard-object-detail-component :instance instance)
          (delay-alternative-component 'standard-object-reference-component
            (setf-expand-reference-to-default-alternative-command
             (make-instance 'standard-object-reference-component :target instance))))))

(def (generic e) make-standard-object-commands (component class instance)
  (:method ((component standard-object-component) (class standard-class) (instance standard-object))
    (list (make-editing-commands component)
          (make-new-instance-command component)))

  (:method ((component standard-object-component) (class prc::persistent-class) (instance prc::persistent-object))
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
                 :action (make-action (not-yet-implemented))))

(def (function e) make-delete-instance-command (component)
  (make-instance 'command-component
                 :icon (icon delete)
                 :visible (delay (not (edited-p component)))
                 :action (make-action (rdbms::with-transaction
                                        (prc::purge-instance (instance-of component))
                                        (setf (component-value-of component) nil)))))

;;;;;;
;;; Standard object detail

(def component standard-object-detail-component (abstract-standard-object-component editable-component detail-component)
  ((class nil :accessor nil :type component)
   (slot-value-group nil :type component))
  (:documentation "Component for an instance of STANDARD-OBJECT in detail"))

(def method (setf component-value-of) :after (new-value (component standard-object-detail-component))
  (with-slots (instance the-class class slot-value-group) component
    (if the-class
        (if class
            (setf (component-value-of class) the-class)
            (setf class (make-viewer-component the-class :default-component-type 'reference-component)))
        (setf class nil))
    (if instance
        (if slot-value-group
            (setf (component-value-of slot-value-group) (standard-object-detail-slots the-class instance)
                  (instance-of slot-value-group) instance
                  (the-class-of slot-value-group) the-class)
            (setf slot-value-group (make-instance 'standard-object-slot-value-group-component :the-class the-class :instance instance :slots (standard-object-detail-slots the-class instance))))
        (setf slot-value-group nil))))

(def generic standard-object-detail-slots (class instance)
  (:method ((class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((class prc::persistent-class) (instance prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((class dmm::entity) (instance prc::persistent-object))
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(def render standard-object-detail-component ()
  (with-slots (class slot-value-group) -self-
    <div (:class "standard-object")
      <span ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-component (abstract-standard-slot-definition-group-component abstract-standard-object-component editable-component)
  ((slot-values nil :type components))
  (:documentation "Component for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITIONs"))

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-group-component))
  (with-slots (instance the-class slots slot-values) component
    (if instance
        (setf slot-values
              (iter (for slot :in slots)
                    (for slot-value-detail = (find slot slot-values :key #'component-value-of))
                    (if slot-value-detail
                        (setf (component-value-of slot-value-detail) slot
                              (instance-of slot-value-detail) instance)
                        (setf slot-value-detail (make-instance 'standard-object-slot-value-detail-component :the-class the-class :instance instance :slot slot)))
                    (collect slot-value-detail)))
        (setf slot-values nil))))

(def render standard-object-slot-value-group-component ()
  (with-slots (slot-values) -self-
    (if slot-values
        <table
          <thead
            <tr
              <th "Name">
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span "There are none">)))

;;;;;;
;;; Abstract object slot value

(def component abstract-standard-object-slot-value-component (abstract-standard-slot-definition-component abstract-standard-object-component)
  ())

;;;;;;
;;; Standard object slot value detail

(def component standard-object-slot-value-detail-component (abstract-standard-object-slot-value-component editable-component)
  ((label nil :type component)
   (value nil :type component))
  (:documentation "Component for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION"))

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-detail-component))
  (with-slots (instance the-class slot label value) component
    (if slot
        (if label
            (setf (component-value-of label) (localized-slot-name slot))
            (setf label (label (localized-slot-name slot))))
        (setf label nil))
    (if instance
        (if value
            (setf (place-of value) (make-slot-value-place instance slot))
            (setf value (make-instance 'place-component :place (make-slot-value-place instance slot))))
        (setf value nil))))

(def render standard-object-slot-value-detail-component ()
  (with-slots (label value) -self-
    <tr (:class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render value)>>))
