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

(def method render :before ((self abstract-standard-object-component))
  (bind ((instance (instance-of self)))
    (if (and (typep instance 'prc::persistent-object)
             (prc::persistent-p instance))
        (prc::revive-instance (instance-of self)))))

;;;;;;
;;; Standard object

(def component standard-object-component (abstract-standard-object-component alternator-component editable-component)
  ()
  (:documentation "Component for an instance of STANDARD-OBJECT in various alternative views"))

(def method (setf component-value-of) :after (new-value (component standard-object-component))
  (with-slots (instance the-class default-component-type alternatives content command-bar) component
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (dolist (alternative alternatives)
                (setf (component-value-of (force alternative)) instance))
              (setf alternatives (list (delay-alternative-component-type 'standard-object-detail-component :instance instance)
                                       (delay-alternative-component 'standard-object-reference-component
                                         (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-reference-component :target instance))))))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) instance)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives)))))
        (setf alternatives (list (delay-alternative-component-type 'null-component))
              content (find-default-alternative-component alternatives)))
    (setf command-bar (make-instance 'command-bar-component
                                     :commands (append (list (make-top-command-component component)
                                                             (make-refresh-command-component component))
                                                       (make-editing-command-components component)
                                                       (make-standard-object-command-components component)
                                                       (make-alternative-command-components component alternatives))))))

(def (function e) make-new-instance-command-component (component)
  (make-instance 'command-component
                 :icon (clone-icon 'new)
                 :visible (delay (not (edited-p component)))
                 :action (make-action (break "TODO:"))))

(def (function e) make-delete-instance-command-component (component)
  (make-instance 'command-component
                 :icon (clone-icon 'delete)
                 :visible (delay (not (edited-p component)))
                 :action (make-action (break "TODO:"))))

(def (function e) make-standard-object-command-components (component)
  (list (make-new-instance-command-component component)
        (make-delete-instance-command-component component)))

(def method add-user-message ((self standard-object-component) message &rest args)
  (apply #'add-user-message (content-of self) message args))

;;;;;;
;;; Standard object detail

(def component standard-object-detail-component (abstract-standard-object-component editable-component detail-component)
  ((user-message-collector :type component)
   (class nil :accessor nil :type component)
   (slot-value-group nil :type component))
  (:documentation "Component for an instance of STANDARD-OBJECT in detail"))

(def method (setf component-value-of) :after (new-value (component standard-object-detail-component))
  (with-slots (instance the-class user-message-collector class slot-value-group) component
    (setf user-message-collector (make-instance 'user-message-collector-component))
    (if the-class
        (if class
            (setf (component-value-of class) the-class)
            (setf class (make-viewer-component the-class :default-component-type 'reference-component)))
        (setf class nil))
    (if instance
        (if slot-value-group
            (setf (component-value-of slot-value-group) the-class
                  (slots-of slot-value-group) (standard-object-detail-slots the-class instance))
            (setf slot-value-group (make-instance 'standard-object-slot-value-group-component :the-class the-class :instance instance :slots (standard-object-detail-slots the-class instance))))
        (setf slot-value-group nil))))

(def generic standard-object-detail-slots (class instance)
  (:method ((class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((class prc::persistent-class) (instance prc::persistent-object))
    (iter (for slot :in (prc::persistent-effective-slots-of class))
          (if (dmm::authorize-operation 'dmm::read-instance-property-operation :-entity- class :-instance- instance :-property- slot)
              (collect slot)))))

(def render standard-object-detail-component ()
  (with-slots (user-message-collector class slot-value-group) -self-
    <div
     ,(render user-message-collector)
      <span "An instance of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

(def method add-user-message ((self standard-object-detail-component) message &rest args)
  (apply #'add-user-message (user-message-collector-of self) message args))

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
            (setf (component-value-of label) (full-symbol-name (slot-definition-name slot)))
            (setf label (make-instance 'label-component :component-value (full-symbol-name (slot-definition-name slot)))))
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
