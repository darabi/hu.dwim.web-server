;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard object

(def component abstract-standard-object-component ()
  ((instance nil :type (or null standard-object))
   (the-class nil :type standard-class)))

(def constructor abstract-standard-object-component ()
  (setf (component-value-of self) (instance-of self)))

(def method component-value-of ((component abstract-standard-object-component))
  (instance-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-component))
  (with-slots (instance the-class) component
    (setf instance new-value)
    (setf the-class (when new-value (class-of new-value)))))

;;;;;;
;;; Standard object

(def component standard-object-component (abstract-standard-object-component alternator-component editable-component)
  ())

(def method (setf component-value-of) :after (new-value (component standard-object-component))
  (with-slots (instance the-class alternatives content command-bar) component
    (if instance
        (progn
          (if alternatives
              (dolist (alternative alternatives)
                (setf (component-value-of (force alternative)) instance))
              (setf alternatives (list (delay-alternative-component-type 'standard-object-detail-component :instance instance)
                                       (delay-alternative-component 'standard-object-reference-component
                                         (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-reference-component :target instance))))))
          (if content
              (setf (component-value-of content) instance)
              (setf content (find-default-alternative-component alternatives))))
        (setf alternatives (list (delay-alternative-component-type 'null-component))
              content (find-default-alternative-component alternatives)))
    (unless command-bar
      (setf command-bar (make-alternator-command-bar-component component alternatives)))))

;;;;;;
;;; Standard object detail

(def component standard-object-detail-component (abstract-standard-object-component editable-component detail-component)
  ((class nil :accessor nil :type component)
   (slot-value-group nil :type component)))

(def method (setf component-value-of) :after (new-value (component standard-object-detail-component))
  (with-slots (instance the-class class slot-value-group) component
    (if the-class
        (if class
            (setf (component-value-of class) the-class)
            (setf class (make-viewer-component the-class)))
        (setf class nil))
    (if instance
        (if slot-value-group
            (setf (component-value-of slot-value-group) the-class
                  (slots-of slot-value-group) (class-slots the-class))
            (setf slot-value-group (make-instance 'standard-object-slot-value-group-component :the-class the-class :instance instance :slots (class-slots the-class))))
        (setf slot-value-group nil))))

(def render standard-object-detail-component ()
  (with-slots (class slot-value-group) self
    <div
      <span "An instance of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-component (abstract-standard-object-component editable-component)
  ((slots nil)
   (slot-values nil :type components)))

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-group-component))
  (with-slots (instance the-class slots slot-values) component
    (if instance
        (setf slot-values
              (iter (for slot :in slots)
                    (for slot-value-detail = (find slot slot-values :key #'component-value-of))
                    (if slot-value-detail
                        (setf (component-value-of slot-value-detail) instance
                              (slot-of slot-value-detail) slot)
                        (setf slot-value-detail (make-instance 'standard-object-slot-value-detail-component :the-class the-class :instance instance :slot slot)))
                    (collect slot-value-detail)))
        (setf slot-values nil))))

(def render standard-object-slot-value-group-component ()
  (with-slots (slot-values) self
    (if slot-values
        <table
          <thead
            <tr
              <th "Name">
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span "There are none">)))

;;;;;;
;;; Standard object slot value detail

(def component standard-object-slot-value-detail-component (abstract-standard-object-component editable-component)
  ((slot)
   (label nil :type component)
   (value nil :type component)))

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
  (with-slots (label value) self
    <tr
      <td ,(render label)>
      <td ,(render value)>>))
