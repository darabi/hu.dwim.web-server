;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object maker

(def (component e) standard-object-maker (standard-class/mixin
                                         maker/abstract
                                         alternator/basic
                                         initargs/mixin
                                         layer-context-capturing/mixin)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object-maker (the-class)
  `(make-instance 'standard-object-maker :the-class ,the-class))

(def layered-method make-title ((self standard-object-maker))
  (title (standard-object-maker.title (localized-class-name (the-class-of self)))))

(def layered-method make-alternatives ((component standard-object-maker) (class standard-class) (prototype standard-object) value)
  (list (delay-alternative-component-with-initargs 'standard-object-detail-maker :the-class class)
        (delay-alternative-reference-component 'standard-object-maker-reference class)))

(def layered-method make-context-menu-items ((component standard-object-maker) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (list (make-create-instance-command component class prototype)) (call-next-method)))

(def (layered-function e) make-create-instance-command (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (command ()
      (icon create)
      (make-component-action component
        (create-instance component (the-class-of component))))))

(def (layered-function e) create-instance (ancestor component class)
  (:method :around ((component standard-object-maker) (class standard-class))
    (with-interaction component
      (call-next-method)))

  (:method ((component standard-object-maker) (class standard-class))
    (place-component-value-of component)))

(def method place-component-value-of ((self standard-object-maker))
  (bind ((class (the-class-of self)))
    (make-instance-using-initargs self class (class-prototype class))))

;;;;;;
;;; Standard object detail maker

(def (component e) standard-object-detail-maker (standard-object-detail-component
                                                standard-class/mixin
                                                maker/abstract)
  ((class-selector nil :type component))
  (:documentation "Maker for an instance of STANDARD-OBJECT in detail."))

(def (macro e) standard-object-detail-maker (class)
  `(make-instance 'standard-object-detail-maker :the-class ,class))

(def (layered-function e) collect-standard-object-detail-maker-classes (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (list* class (subclasses class))))

(def refresh-component standard-object-detail-maker
  (bind (((:slots class-selector the-class slot-value-groups) -self-)
         (selectable-classes (collect-standard-object-detail-maker-classes -self- the-class (class-prototype the-class))))
    (if (length= selectable-classes 1)
        (setf class-selector nil)
        (if class-selector
            (setf (possible-values-of class-selector) selectable-classes)
            (setf class-selector (make-class-selector selectable-classes))))
    (bind ((selected-class (if class-selector
                               (component-value-of class-selector)
                               (first selectable-classes))))
      (setf slot-value-groups (bind ((prototype (class-prototype selected-class))
                                     (slots (collect-standard-object-detail-maker-slots -self- selected-class prototype))
                                     (slot-groups (collect-standard-object-detail-slot-groups -self- selected-class prototype slots)))
                                (iter (for (name . slot-group) :in slot-groups)
                                      (when slot-group
                                        (for slot-value-group = (find-slot-value-group-component slot-group slot-value-groups))
                                        (if slot-value-group
                                            (setf (component-value-of slot-value-group) slot-group
                                                  (the-class-of slot-value-group) selected-class
                                                  (name-of slot-value-group) name)
                                            (setf slot-value-group (make-instance 'standard-object-slot-value-group-maker
                                                                                  :the-class selected-class
                                                                                  :slots slot-group
                                                                                  :name name)))
                                        (collect slot-value-group))))))))

(def (layered-function e) collect-standard-object-detail-maker-slots (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (class-slots class)))

(def render-xhtml standard-object-detail-maker
  (bind (((:read-only-slots class-selector slot-value-groups id) -self-))
    <div (:id ,id)
         <table (:class "slot-table")
           ,(when class-selector
                  <tbody <tr <td ,#"standard-object-detail-maker.class-selector-label">
                             <td ,(render-component class-selector)>>>)
           ,(foreach #'render-component slot-value-groups)>>))

;;;;;;
;;; Standard object slot value group maker

(def (component e) standard-object-slot-value-group-maker (standard-object-slot-value-group-component maker/abstract)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITIONs."))

(def refresh-component standard-object-slot-value-group-maker
  (bind (((:slots the-class slots slot-values) -self-))
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value-component = (find-slot-value-component slot slot-values))
                (if slot-value-component
                    (setf (slot-of slot-value-component) slot)
                    (setf slot-value-component (make-standard-object-slot-value-maker -self- the-class (class-prototype the-class) slot)))
                (collect slot-value-component)))))

(def (generic e) make-standard-object-slot-value-maker (component class instance slot)
  (:method ((component standard-object-slot-value-group-maker) (class standard-class) (prototype standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-maker :the-class class :slot slot)))

;;;;;
;;; Standard object slot value maker

(def (component e) standard-object-slot-value-maker (standard-object-slot-value/inspector maker/abstract)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def refresh-component standard-object-slot-value-maker ()
  (bind (((:slots slot label value) -self-))
    (setf label (localized-slot-name slot)
          value (make-place-maker (slot-type slot) :name (slot-definition-name slot) :initform (slot-definition-initform slot)))))

;;;;;;
;;; Standard object place maker

(def (component e) standard-object-place-maker (place-maker)
  ()
  (:documentation "Maker for a place of an instance of STANDARD-OBJECT and unit types."))

(def method make-place-component-content ((self standard-object-place-maker))
  (make-instance 'unbound-component))

(def method make-place-component-command-bar ((self standard-object-place-maker))
  (make-instance 'command-bar/basic :commands (optional-list (make-set-place-to-nil-command self)
                                                             (when (initform-of self)
                                                               (make-set-place-to-unbound-command self))
                                                             (make-set-place-to-find-instance-command self)
                                                             (make-set-place-to-new-instance-command self))))

;;;;;;
;;; Execute maker

(def (generic e) make-instance-using-initargs (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (apply #'make-instance (aif (class-selector-of (content-of component))
                                (component-value-of it)
                                (the-class-of (content-of component)))
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
    (bind ((content (content-of component)))
      (unless (typep content 'unbound-component)
        (bind ((slot (slot-of (parent-component-of component)))
               (value (place-component-value-of content))
               (initarg (first (slot-definition-initargs slot))))
          (list initarg value)))))

  (:method ((component primitive-component))
    (component-value-of component))

  (:method (component)
    nil))
