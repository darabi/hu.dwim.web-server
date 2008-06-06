;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object maker

(def component standard-object-maker-component (abstract-standard-class-component alternator-component editable-component user-message-collector-component-mixin)
  ())

(def method (setf component-value-of) :after (new-value (component standard-object-maker-component))
  (with-slots (the-class default-component-type alternatives content command-bar) component
    (if the-class
        (progn
          (if alternatives
              (dolist (alternative alternatives)
                (setf (component-value-of alternative) the-class))
              (setf alternatives (list (delay-alternative-component-type 'standard-object-maker-detail-component :the-class the-class)
                                       (delay-alternative-component 'standard-object-maker-reference-component
                                         (setf-expand-reference-to-default-alternative-command (make-instance 'standard-object-maker-reference-component :target the-class))))))
          (if content
              (setf (component-value-of content) the-class)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-instance 'command-bar-component
                                           :commands (append (make-standard-object-maker-commands component the-class)
                                                             (make-alternative-commands component alternatives)))))
        (setf alternatives nil
              content nil))))

(def render standard-object-maker-component ()
  <div ,(render-user-messages -self-)
       ,(call-next-method)>)

(def generic make-standard-object-maker-commands (component class)
  (:method ((component standard-object-maker-component) (class standard-class))
    (list (make-create-instance-command component))))

(def (function e) make-create-instance-command (component)
  (make-instance 'command-component
                 :icon (icon create)
                 :visible (delay (edited-p component))
                 ;; TODO: put transaction here?! how do we dispatch
                 :action (make-action (execute-maker component (the-class-of component))
                                      (add-user-information component "Az új objektum sikeresen létrehozva"))))

;;;;;;
;;; Standard object maker detail

(def component standard-object-maker-detail-component (abstract-standard-class-component alternator-component editable-component)
  ((class :accessor nil :type component)
   (slot-value-group :type component)))

(def constructor standard-object-maker-detail-component ()
  (with-slots (the-class class slot-value-group command-bar) -self-
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-group (make-instance 'standard-object-slot-value-group-maker-component
                                          :slots (standard-object-maker-detail-slots the-class)))))

(def generic standard-object-maker-detail-slots (class)
  (:method ((class standard-class))
    (class-slots class))

  (:method ((class prc::persistent-class))
    (iter (for slot :in (prc::persistent-effective-slots-of class))
          (if (dmm::authorize-operation 'dmm::create-entity-property-operation :-entity- class :-property- slot)
              (collect slot)))))

(def render standard-object-maker-detail-component ()
  (with-slots (the-class class slots-values slot-value-group command-bar) -self-
    <div
      <span "Making an instance of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-maker-component (abstract-standard-slot-definition-group-component)
  ((slot-values nil :type components)))

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-group-maker-component))
  (with-slots (the-class slots slot-values) component
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value-detail = (find slot slot-values :key #'component-value-of))
                (if slot-value-detail
                    (setf (slot-of slot-value-detail) slot)
                    (setf slot-value-detail (make-instance 'standard-object-slot-value-maker-detail-component :the-class the-class :slot slot)))
                (collect slot-value-detail)))))

(def render standard-object-slot-value-group-maker-component ()
  (with-slots (slot-values) -self-
    (if slot-values
        <table
          <thead
            <tr
              <th "Name">
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span "There are none">)))

;;;;;
;;; Standard object slot value maker detail

(def component standard-object-slot-value-maker-detail-component (abstract-standard-slot-definition-component)
  ((label nil :type component)
   (value nil :type component)))

(def constructor standard-object-slot-value-maker-detail-component ()
  (with-slots (slot label value) -self-
    (setf label (make-instance 'label-component :component-value (full-symbol-name (slot-definition-name slot)))
          value (make-maker-component (slot-type slot) :default-component-type 'reference-component))))

(def render standard-object-slot-value-maker-detail-component ()
  (with-slots (label value) -self-
    <tr (:class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render value)>>))

;;;;;;
;;; Execute maker

(def generic execute-maker (component class)
  (:method ((component standard-object-maker-component) (class standard-class))
    (execute-maker* component))

  (:method ((component standard-object-maker-component) (class prc::persistent-class))
    ;; TODO: move to action and kill this generism
    (rdbms::with-transaction
      (call-next-method))))

(def generic execute-maker* (component)
  (:method ((component standard-object-maker-component))
    (apply #'make-instance (the-class-of component)
           (execute-maker* (content-of component))))

  (:method ((component standard-object-maker-detail-component))
    (execute-maker* (slot-value-group-of component)))

  (:method ((component standard-object-slot-value-group-maker-component))
    (iter (for slot-value :in (slot-values-of component))
          (appending (execute-maker* slot-value))))

  (:method ((component standard-object-slot-value-maker-detail-component))
    (bind ((slot (slot-of component))
           (value (value-of component)))
      ;; TODO: recurse
      (when (typep value 'atomic-component)
        (list (first (slot-definition-initargs slot))
              (execute-maker* value)))))

  (:method ((component atomic-component))
    (component-value-of component)))
