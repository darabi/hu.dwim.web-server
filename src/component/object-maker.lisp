;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object maker

(def component standard-object-maker (abstract-standard-class-component
                                      maker-component
                                      alternator-component
                                      user-message-collector-component-mixin
                                      remote-identity-component-mixin
                                      initargs-component-mixin
                                      layer-context-capturing-component-mixin
                                      recursion-point-component)
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
                                                         (make-standard-commands self the-class (class-prototype the-class)))))
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
          (progn
            <div (:id ,id :class "standard-object-maker")
              ,(body)>
            `js(on-load (wui.setup-standard-object-maker ,id)))))))

(def (layered-function e) make-standard-object-maker-alternatives (component class prototype)
  (:method ((component standard-object-maker) (class standard-class) (prototype standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-maker :the-class class)
          (delay-alternative-reference-component 'standard-object-maker-reference class))))

(def layered-method make-standard-commands ((component standard-object-maker) (class standard-class) (prototype standard-object))
  (append (list (make-create-instance-command component)) (call-next-method)))

(def (function e) make-create-instance-command (component)
  (command (icon create)
           (make-action
             (execute-create-instance (find-ancestor-component-with-type (parent-component-of component) 'recursion-point-component)
                                      component (the-class-of component)))))

(def (layered-function e) execute-create-instance (ancestor component class)
  (:method ((ancestor recursion-point-component) (component standard-object-maker) (class standard-class))
    (place-component-value-of component)))

(def method place-component-value-of ((self standard-object-maker))
  (bind ((class (the-class-of self)))
    (make-instance-using-initargs self class (class-prototype class))))

;;;;;;
;;; Standard object detail maker

(def component standard-object-detail-maker (standard-object-detail-component
                                             abstract-standard-class-component
                                             maker-component
                                             title-component-mixin)
  ((class-selector nil :type component))
  (:documentation "Maker for an instance of STANDARD-OBJECT in detail."))

(def (macro e) standard-object-detail-maker (class)
  `(make-instance 'standard-object-detail-maker :the-class ,class))

(def (layered-function e) collect-standard-object-detail-maker-classes (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (list* class (subclasses class))))

(def method refresh-component ((self standard-object-detail-maker))
  (with-slots (class class-selector the-class slot-value-groups) self
    (bind ((selectable-classes (collect-standard-object-detail-maker-classes self the-class (class-prototype the-class))))
      (if (length= selectable-classes 1)
          (setf class-selector nil)
          (if class-selector
              (setf (possible-values-of class-selector) selectable-classes)
              (setf class-selector (make-class-selector selectable-classes))))
      (bind ((selected-class (if class-selector
                                 (component-value-of class-selector)
                                 (first selectable-classes))))
        (setf class (make-standard-object-detail-maker-class self the-class (class-prototype the-class))
              slot-value-groups (bind ((prototype (class-prototype selected-class))
                                       (slots (collect-standard-object-detail-maker-slots self selected-class prototype))
                                       (slot-groups (collect-standard-object-detail-slot-groups self selected-class prototype slots)))
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
                                          (collect slot-value-group)))))))))

(def (layered-function e) make-standard-object-detail-maker-class (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (localized-class-name class)))

(def (layered-function e) collect-standard-object-detail-maker-slots (component class prototype)
  (:method ((component standard-object-detail-maker) (class standard-class) (prototype standard-object))
    (class-slots class)))

(def render standard-object-detail-maker ()
  (bind (((:read-only-slots class-selector slot-value-groups id) -self-))
    <div (:id ,id)
         ,(render-title -self-)
         <table (:class "slot-table")
           ,(when class-selector
                  <tbody <tr <td ,#"standard-object-detail-maker.class-selector-label">
                             <td ,(render class-selector)>>>)
           ,(foreach #'render slot-value-groups)>>))

(def layered-method render-title ((self standard-object-detail-maker))
  (standard-object-detail-maker.title (slot-value self 'class)))

(def resources en
  (standard-object-detail-maker.title (class)
    `xml,"Creating an instance of" (render class))
  (standard-object-detail-maker.class-selector-label "Class"))

(def resources hu
  (standard-object-detail-maker.title (class)
    `xml,"Egy új " (render class) `xml," felvétele")
  (standard-object-detail-maker.class-selector-label "Típus"))

;;;;;;
;;; Standard object slot value group maker

(def component standard-object-slot-value-group-maker (standard-object-slot-value-group-component maker-component)
  ()
  (:documentation "Maker for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITIONs."))

(def method refresh-component ((self standard-object-slot-value-group-maker))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value-component = (find-slot-value-component slot slot-values))
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
  (:documentation "Maker for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def method refresh-component ((self standard-object-slot-value-maker)) ()
  (with-slots (slot label value) self
    (setf label (localized-slot-name slot)
          value (make-place-maker (slot-type slot) :name (slot-definition-name slot) :initform (slot-definition-initform slot)))))

;;;;;;
;;; Standard object place maker

(def component standard-object-place-maker (place-maker)
  ()
  (:documentation "Maker for a place of an instance of STANDARD-OBJECT and unit types."))

(def method make-place-component-content ((self standard-object-place-maker))
  (make-instance 'unbound-component))

(def method make-place-component-command-bar ((self standard-object-place-maker))
  (make-instance 'command-bar-component :commands (optional-list (make-set-place-to-nil-command self)
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
    (bind ((content-component (content-of component)))
      (unless (typep content-component 'unbound-component)
        (list (first (slot-definition-initargs (slot-of (parent-component-of component))))
              (place-component-value-of content-component)))))

  (:method ((component primitive-component))
    (component-value-of component))

  (:method (component)
    nil))
