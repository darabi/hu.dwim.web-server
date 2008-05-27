;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object

(def component standard-object-component (content-component #+nil alternator-component editable-component)
  ((instance :type (or null standard-object))
   (the-class :type standard-class)))

(def constructor standard-object-component ()
  (with-slots (instance the-class alternatives content) self
    (setf the-class (class-of instance)
          #+nil alternatives
          #+nil (list (delay-alternative-component-type 'standard-object-detail-component :the-class the-class :instance instance)
                      (delay-alternative-component-type 'standard-object-reference-component :target instance)))))

(def component standard-object-detail-component (editable-component)
  ((instance)
   (the-class)
   (class :accessor nil :type component)
   (slot-value-group :type component)))

(def constructor standard-object-detail-component ()
  (with-slots (instance the-class class slot-value-group) self
    the-class (class-of instance)
    class (make-viewer-component the-class)
    slot-value-group (make-instance 'standard-object-slot-value-group-component :the-class the-class :instance instance :slots (class-slots the-class))))

(def render standard-object-detail-component ()
  (with-slots (class slot-value-group) self
    <div
      <span "An instance of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

(def component standard-object-slot-value-group-component (editable-component)
  ((instance)
   (the-class)
   (slots)
   (slot-value-details :type components)))

(def constructor standard-object-slot-value-group-component ()
  (with-slots (instance the-class slots slot-value-details) self
    (setf the-class (class-of instance)
          slot-value-details (mapcar (lambda (slot)
                                       (make-instance 'standard-object-slot-value-detail-component :the-class the-class :instance instance :slot slot))
                                     slots))))

(def render standard-object-slot-value-group-component ()
  (with-slots (slot-value-details) self
    (if slot-value-details
        <table
          <thead
            <tr
              <th "Name">
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-value-details)>>
        <span "There are none">)))

(def component standard-object-slot-value-detail-component (editable-component)
  ((instance)
   (the-class)
   (slot)
   (label :type component)
   (content :type component)))

(def constructor standard-object-slot-value-detail-component ()
  (with-slots (instance the-class slot label content) self
    (setf the-class (class-of instance)
          label (make-instance 'string-component :value (full-symbol-name (slot-definition-name slot)))
          content (make-instance 'place-component :place (if instance
                                                             (make-slot-value-place instance slot)
                                                             (make-phantom-slot-value-place the-class slot))))))

(def render standard-object-slot-value-detail-component ()
  (with-slots (label content) self
    <tr
      <td ,(render label)>
      <td ,(render content)>>))
