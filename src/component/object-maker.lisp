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
  (bind (((:read-only-slots id content) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (call-next-method)))
      (if (typep content 'reference-component)
          <span (:id ,id :class "standard-object-maker")
            ,(body)>
          <div (:id ,id :class "standard-object-maker")
            ,(body)>))))

(def (layered-function e) make-standard-object-maker-alternatives (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-maker :the-class class)
          (delay-alternative-reference-component 'standard-object-maker-reference class))))

(def (layered-function e) make-standard-object-maker-commands (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (list (make-create-instance-command component))))

(def (function e) make-create-instance-command (component)
  (make-instance 'command-component
                 :icon (icon create)
                 :action (make-action
                           (execute-create-instance component (the-class-of component)))))

(def (layered-function e) execute-create-instance (component class)
  (:method ((component standard-object-maker) (class standard-class))
    (place-component-value-of component)))

(def method place-component-value-of ((self standard-object-maker))
  (bind ((class (the-class-of self)))
    (make-instance-using-initargs self class (class-prototype class))))

;;;;;;
;;; Standard object detail maker

(def component standard-object-detail-maker (abstract-standard-class-component
                                             maker-component
                                             remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (class-selector nil :type component)
   (slot-value-groups nil :type components))
  (:documentation "Maker for an instance of STANDARD-OBJECT in detail"))

(def (macro e) standard-object-detail-maker (class)
  `(make-instance 'standard-object-detail-maker :the-class ,class))

(def constructor standard-object-detail-maker ()
  (with-slots (the-class class-selector) -self-
    (setf class-selector
          (when-bind subclasses (subclasses the-class)
            (make-instance 'member-component
                           :edited #t
                           :allow-nil-value #t
                           :component-value the-class
                           :possible-values subclasses)))))

(def function find-selected-class (component)
  (bind ((class-selector (class-selector-of component)))
    (aif (and class-selector
              (component-value-of class-selector))
         it
         (the-class-of component))))

(def method refresh-component ((self standard-object-detail-maker))
  (with-slots (class class-selector the-class slot-value-groups) self
    (bind ((selected-class (find-selected-class self)))
      (setf class (make-standard-object-detail-maker-class self the-class (class-prototype the-class))
            slot-value-groups (bind ((prototype (class-prototype selected-class))
                                     (slots (collect-standard-object-detail-maker-slots self selected-class prototype))
                                     (slot-groups (collect-standard-object-detail-maker-slot-groups self selected-class prototype slots)))
                                (iter (for slot-group :in slot-groups)
                                      (when slot-group
                                        (for slot-value-group = (find slot-group slot-value-groups :key 'slots-of :test 'equal))
                                        (if slot-value-group
                                            (setf (component-value-of slot-value-group) slot-group
                                                  (the-class-of slot-value-group) selected-class)
                                            (setf slot-value-group (make-instance 'standard-object-slot-value-group-maker
                                                                                  :the-class selected-class
                                                                                  :slots slot-group)))
                                        (collect slot-value-group))))))))

(def (layered-function e) make-standard-object-detail-maker-class (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (localized-class-name class)))

(def (layered-function e) collect-standard-object-detail-maker-slot-groups (component class prototype slots)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object) (slots list))
    (list slots)))

(def (layered-function e) collect-standard-object-detail-maker-slots (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (class-slots class)))

(def render standard-object-detail-maker ()
  (bind (((:read-only-slots class-selector class slot-value-groups id) -self-))
    <div (:id ,id)
         <span ,(standard-object-detail-maker.instance class)>
         ,(when class-selector
                <div "Narrow down to "
                     ,(render class-selector)
                     ,(render (command (icon refresh)
                                       (make-action
                                         (setf (outdated-p -self-) #t))))>)
         <div <h3 ,#"standard-object-detail-maker.slots">
              <table ,(map nil #'render slot-value-groups)>>>))

(defresources en
  (standard-object-detail-maker.instance (class)
    <span "Creating an instance of" ,(render class)>)
  (standard-object-detail-maker.slots "Slots"))

(defresources hu
  (standard-object-detail-maker.instance (class)
    <span "Egy új " ,(render class) " felvétele">)
  (standard-object-detail-maker.slots "Tulajdonságok"))

;;;;;;
;;; Standard object slot value group maker

(def component standard-object-slot-value-group-maker (standard-object-slot-value-group-component maker-component)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITIONs"))

(def method refresh-component ((self standard-object-slot-value-group-maker))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value-component = (find slot slot-values :key #'component-value-of))
                (if slot-value-component
                    (setf (slot-of slot-value-component) slot)
                    (setf slot-value-component (make-standard-object-slot-value-maker self the-class (class-prototype the-class) slot)))
                (collect slot-value-component)))))

(def (generic e) make-standard-object-slot-value-maker (component class instance slot)
  (:method ((component standard-object-slot-value-group-maker) (class standard-class) (prototype standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-maker :the-class class :slot slot)))

;;;;;
;;; Standard object slot value maker

(def component standard-object-slot-value-maker (standard-object-slot-value-component maker-component)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION"))

(def method refresh-component ((self standard-object-slot-value-maker)) ()
  (with-slots (slot label value) self
    (setf label (label (localized-slot-name slot)))
    (setf value (make-place-maker (slot-type slot) :initform (slot-definition-initform slot)))))

;;;;;;
;;; Standard object place maker

(def component standard-object-place-maker (place-maker)
  ()
  (:documentation "Maker for a place of an instance of STANDARD-OBJECT and unit types."))

(def method make-place-component-content ((self standard-object-place-maker))
  (make-instance 'unbound-component))

(def method make-place-component-command-bar ((self standard-object-place-maker))
  (bind ((type (the-type-of self)))
    (make-instance 'command-bar-component :commands (optional-list (make-set-place-to-nil-command self)
                                                                   (when (initform-of self)
                                                                     (make-set-place-to-unbound-command self))
                                                                   (make-set-place-to-find-instance-command self)
                                                                   (make-set-place-to-new-instance-command self)))))

;;;;;;
;;; Execute maker

(def (generic e) make-instance-using-initargs (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (apply #'make-instance (find-selected-class (content-of component))
           (collect-make-instance-initargs component))))

(def (generic e) collect-make-instance-initargs (component)
  (:method ((component standard-object-maker))
    (collect-make-instance-initargs (content-of component)))

  (:method ((component standard-object-detail-maker))
    (mappend #'collect-make-instance-initargs (slot-value-groups-of component)))

  (:method ((component standard-object-slot-value-group-maker))
    (iter (for slot-value :in (slot-values-of component))
          (appending (collect-make-instance-initargs slot-value))))

  (:method ((component standard-object-slot-value-maker))
    (collect-make-instance-initargs (value-of component)))

  (:method ((component place-maker))
    (bind ((content-component (content-of component)))
      (unless (typep content-component 'unbound-component)
        (list (first (slot-definition-initargs (slot-of (parent-component-of component))))
              (place-component-value-of content-component)))))

  (:method ((component atomic-component))
    (component-value-of component))

  (:method (component)
    nil))
