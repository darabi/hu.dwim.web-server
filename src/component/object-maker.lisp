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
                                      remote-identity-component-mixin
                                      initargs-component-mixin
                                      layered-component-mixin)
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
          (delay-alternative-reference-component 'standard-object-maker-reference class))))

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
                             (execute-create-instance component class (class-prototype class))))))

;;;;;;
;;; Standard object maker detail

(def component standard-object-detail-maker (abstract-standard-class-component
                                             maker-component
                                             editable-component
                                             remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (slot-value-groups nil :type components)))

(def (macro e) standard-object-detail-maker (class)
  `(make-instance 'standard-object-detail-maker :the-class ,class))

(def method refresh-component ((self standard-object-detail-maker))
  (with-slots (class the-class slot-value-groups) self
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-groups (bind ((prototype (class-prototype the-class))
                                   (slots (collect-standard-object-detail-maker-slots self the-class prototype))
                                   (slot-groups (collect-standard-object-detail-maker-slot-value-groups self the-class prototype slots)))
                              (iter (for slot-group :in slot-groups)
                                    (when slot-group
                                      (for slot-value-group = (find slot-group slot-value-groups :key 'slots-of :test 'equal))
                                      (if slot-value-group
                                          (setf (component-value-of slot-value-group) slot-group
                                                (the-class-of slot-value-group) the-class)
                                          (setf slot-value-group (make-instance 'standard-object-slot-value-group-maker
                                                                                :the-class the-class
                                                                                :slots slot-group)))
                                      (collect slot-value-group)))))))

(def (generic e) collect-standard-object-detail-maker-slot-value-groups (component class prototype slots)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object) (slots list))
    slots)

  (:method ((component standard-object-detail-maker) (class dmm::entity) (prototype prc::persistent-object) (slots list))
    (partition slots #'dmm::primary-p (constantly #t))))

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
  (with-slots (the-class class slots-values slot-value-groups command-bar id) -self-
    <div (:id ,id)
         <span ,#"standard-object-detail-maker.instance" " " ,(render class)>
         <div <h3 ,#"standard-object-detail-maker.slots">
              ,(map nil #'render slot-value-groups)>>))

(defresources en
  (standard-object-detail-maker.instance "Creating an instance of")
  (standard-object-detail-maker.slots "Slots"))

(defresources hu
  (standard-object-detail-maker.instance "Egy új ")
  (standard-object-detail-maker.slots "Tulajdonságok"))

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
                    (setf slot-value-detail (make-standard-object-slot-value-detail-maker self the-class (class-prototype the-class) slot)))
                (collect slot-value-detail)))))

(def (generic e) make-standard-object-slot-value-detail-maker (component class instance slot)
  (:method ((component standard-object-slot-value-group-maker) (class standard-class) (prototype standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-detail-maker :the-class class :slot slot)))

(def render standard-object-slot-value-group-maker ()
  (with-slots (slot-values id) -self-
    (if slot-values
        <table (:id ,id :class "slot-value-group")
          <thead <tr <th ,#"standard-object-slot-value-group-maker.column.name">
                     <th ,#"standard-object-slot-value-group-maker.column.value">>>
          <tbody ,@(map nil #'render slot-values)>>
        <span (:id ,id) ,#"there-are-none">)))

(defresources en
  (standard-object-slot-value-group-maker.column.name "Name")
  (standard-object-slot-value-group-maker.column.value "Value"))

(defresources hu
  (standard-object-slot-value-group-maker.column.name "Név")
  (standard-object-slot-value-group-maker.column.value "Érték"))

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
        <td (:class "slot-value-detail-label")
            ,(render label)>
        <td (:class "slot-value-detail-label")
            ,(render value)>>))

;;;;;;
;;; Execute maker

(def (generic e) execute-create-instance (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (apply #'make-instance (the-class-of component)
           (collect-make-instance-initargs component)))

  (:method :around ((component standard-object-maker) (class prc::persistent-class) (prototype prc::persistent-object))
    ;; TODO: move to action and kill this generism
    (rdbms::with-transaction
      (call-next-method)
      (when (eq :commit (rdbms::terminal-action-of rdbms::*transaction*))
        (add-user-information component "Az új ~A létrehozása sikerült" (localized-class-name (the-class-of component)))))))

(def (generic e) collect-make-instance-initargs (component)
  (:method ((component standard-object-maker))
    (collect-make-instance-initargs (content-of component)))

  (:method ((component standard-object-detail-maker))
    (mappend #'collect-make-instance-initargs (slot-value-groups-of component)))

  (:method ((component standard-object-slot-value-group-maker))
    (iter (for slot-value :in (slot-values-of component))
          (appending (collect-make-instance-initargs slot-value))))

  (:method ((component standard-object-slot-value-detail-maker))
    (bind ((slot (slot-of component))
           (value (value-of component)))
      ;; TODO: recurse
      (when (typep value 'atomic-component)
        (list (first (slot-definition-initargs slot))
              (collect-make-instance-initargs value)))))

  (:method ((component atomic-component))
    (component-value-of component)))
