;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Maker

(def component maker-component ()
  ())

;;;;;;
;;; Standard object maker

(def component standard-object-maker (abstract-standard-class-component
                                      maker-component
                                      editable-component
                                      alternator-component
                                      user-message-collector-component-mixin
                                      remote-identity-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-maker-alternatives)
  (:documentation "Maker for an instance of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object-maker (the-class)
  `(make-instance 'standard-object-maker :the-class ,the-class))

(def method refresh-component ((self standard-object-maker))
  (with-slots (the-class default-component-type alternatives content command-bar) self
    (if the-class
        (progn
          (if alternatives
              (setf (component-value-for-alternatives self) the-class)
              (setf alternatives (funcall (alternatives-factory-of self) self the-class (class-prototype the-class))))
          (if content
              (setf (component-value-of content) the-class)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-alternator-command-bar self alternatives
                                                         (append (list (make-open-in-new-frame-command self)
                                                                       (make-top-command self)
                                                                       (make-refresh-command self))
                                                                 (make-standard-object-maker-commands self the-class (class-prototype the-class))))))
        (setf alternatives nil
              content nil))))

(def render standard-object-maker ()
  (with-slots (id) -self-
    <div (:id ,id)
         ,(render-user-messages -self-)
         ,(call-next-method)>))

(def (generic e) make-standard-object-maker-alternatives (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-maker :the-class class)
          (delay-alternative-reference-component 'standard-object-maker-reference-component class))))

(def (generic e) make-standard-object-maker-commands (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (list (make-create-instance-command component))))

(def (function e) make-create-instance-command (component)
  (make-instance 'command-component
                 :icon (icon create)
                 :visible (delay (edited-p component))
                 ;; TODO: put transaction here?! how do we dispatch
                 :action (make-action
                           (bind ((class (the-class-of component)))
                             (execute-create-instance component class (class-prototype class))
                             (add-user-information component "Az új objektum sikeresen létrehozva")))))

;;;;;;
;;; Standard object maker detail

(def component standard-object-detail-maker (abstract-standard-class-component
                                             maker-component
                                             alternator-component
                                             editable-component
                                             remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (slot-value-group nil :type component)))

(def method refresh-component ((self standard-object-detail-maker))
  (with-slots (class the-class slot-value-group) self
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-group (make-instance 'standard-object-slot-value-group-maker
                                          :slots (collect-standard-object-detail-maker-slots self the-class (class-prototype the-class))))))

(def (generic e) collect-standard-object-detail-maker-slots (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (class-slots class))

  (:method ((component standard-object-detail-maker) (class prc::persistent-class) (prototype prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-detail-maker) (class dmm::entity) (prototype prc::persistent-object))
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::create-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(def render standard-object-detail-maker ()
  (with-slots (the-class class slots-values slot-value-group command-bar id) -self-
    <div (:id ,id)
      <span "Making an instance of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-maker (abstract-standard-slot-definition-group-component
                                                       maker-component
                                                       remote-identity-component-mixin)
  ((slot-values nil :type components)))

(def method refresh-component ((self standard-object-slot-value-group-maker))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value-detail = (find slot slot-values :key #'component-value-of))
                (if slot-value-detail
                    (setf (slot-of slot-value-detail) slot)
                    (setf slot-value-detail (make-instance 'standard-object-slot-value-detail-maker :the-class the-class :slot slot)))
                (collect slot-value-detail)))))

(def render standard-object-slot-value-group-maker ()
  (with-slots (slot-values id) -self-
    (if slot-values
        <table (:id ,id)
          <thead
            <tr
              <th "Name">
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span (:id ,id) "There are none">)))

;;;;;
;;; Standard object slot value maker detail

(def component standard-object-slot-value-detail-maker (abstract-standard-slot-definition-component
                                                        maker-component
                                                        remote-identity-component-mixin)
  ((label nil :type component)
   (value nil :type component)))

(def constructor standard-object-slot-value-detail-maker ()
  (with-slots (slot label value) -self-
    (setf label (label (localized-slot-name slot)))
    (setf value (make-maker-component (slot-type slot) :default-component-type 'reference-component))))

(def render standard-object-slot-value-detail-maker ()
  (with-slots (label value id) -self-
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render value)>>))

;;;;;;
;;; Execute maker

(def (generic e) execute-create-instance (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (apply #'make-instance (the-class-of component)
           (collect-make-instance-form component)))

  (:method ((component standard-object-maker) (class prc::persistent-class) (prototype prc::persistent-object))
    ;; TODO: move to action and kill this generism
    (rdbms::with-transaction
      (call-next-method))))

(def generic collect-make-instance-form (component)
  (:method ((component standard-object-maker))
    (collect-make-instance-form (content-of component)))

  (:method ((component standard-object-detail-maker))
    (collect-make-instance-form (slot-value-group-of component)))

  (:method ((component standard-object-slot-value-group-maker))
    (iter (for slot-value :in (slot-values-of component))
          (appending (collect-make-instance-form slot-value))))

  (:method ((component standard-object-slot-value-detail-maker))
    (bind ((slot (slot-of component))
           (value (value-of component)))
      ;; TODO: recurse
      (when (typep value 'atomic-component)
        (list (first (slot-definition-initargs slot))
              (collect-make-instance-form value)))))

  (:method ((component atomic-component))
    (component-value-of component)))
