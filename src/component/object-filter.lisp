;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def component standard-object-filter (abstract-standard-class-component
                                       filter-component
                                       alternator-component
                                       user-message-collector-component-mixin
                                       remote-identity-component-mixin
                                       initargs-component-mixin
                                       layered-component-mixin)
  ((result
    (make-instance 'empty-component)
    :type component)
   (result-component-factory
    #'make-standard-object-filter-result-inspector
    :type function))
  (:default-initargs :alternatives-factory #'make-standard-object-filter-alternatives)
  (:documentation "Filter for instances of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object-filter (the-class)
  `(make-instance 'standard-object-filter :the-class ,the-class))

(def method refresh-component ((self standard-object-filter))
  (with-slots (result the-class default-component-type alternatives content command-bar) self
    (if the-class
        (setf alternatives (funcall (alternatives-factory-of self) self the-class (class-prototype the-class))
              content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives)))
        (setf alternatives nil
              content nil))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (append (list (make-open-in-new-frame-command self)
                                                                 (make-top-command self))
                                                           (make-standard-object-filter-commands self the-class (class-prototype the-class)))))))

(def (layered-function e) make-standard-object-filter-alternatives (component class prototype)
  (:method ((component standard-object-filter) (class standard-class) (prototype standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-filter :the-class class)
          (delay-alternative-reference-component 'standard-object-filter-reference class))))

(def (layered-function e) make-standard-object-filter-commands (component class prototype)
  (:method ((component standard-object-filter) (class standard-class) (prototype standard-object))
    (list (make-filter-instances-command component (delay (result-of component))))))

(def render standard-object-filter ()
  (bind (((:read-only-slots result content command-bar id) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (render content)
             (unless (typep content '(or reference-component primitive-component))
               (render command-bar)
               (render result))))
      (if (typep content 'reference-component)
          <span (:id ,id :class "standard-object-filter")
            ,(body)>
          <div (:id ,id :class "standard-object-filter")
            ,(body)>))))

;;;;;;
;;; Standard object detail filter

(def component standard-object-detail-filter (abstract-standard-class-component
                                              filter-component
                                              detail-component
                                              remote-identity-component-mixin)
  ((class nil :accessor nil :type component)
   (class-selector nil :type component)
   (slot-value-groups nil :type component)))

(def (macro e) standard-object-detail-filter (class)
  `(make-instance 'standard-object-detail-filter :the-class ,class))

(def constructor standard-object-detail-filter ()
  (with-slots (the-class class-selector) -self-
    (setf class-selector
          (when-bind subclasses (subclasses the-class)
            (make-instance 'member-inspector
                           :edited #t
                           :allow-nil-value #t
                           :component-value the-class
                           :possible-values subclasses)))))

(def method refresh-component ((self standard-object-detail-filter))
  (with-slots (class class-selector the-class slot-value-groups command-bar) self
    (bind ((selected-class (or (when class-selector (component-value-of class-selector))
                               the-class)))
      (setf class (make-standard-object-detail-filter-class self the-class (class-prototype the-class))
            slot-value-groups (bind ((prototype (class-prototype selected-class))
                                     (slots (collect-standard-object-detail-filter-slots self selected-class prototype))
                                     (slot-groups (collect-standard-object-detail-filter-slot-groups self selected-class prototype slots)))
                                (iter (for slot-group :in slot-groups)
                                      (when slot-group
                                        (for slot-value-group = (find slot-group slot-value-groups :key 'slots-of :test 'equal))
                                        (if slot-value-group
                                            (setf (component-value-of slot-value-group) slot-group
                                                  (the-class-of slot-value-group) selected-class)
                                            (setf slot-value-group (make-instance 'standard-object-slot-value-group-filter
                                                                                  :the-class selected-class
                                                                                  :slots slot-group)))
                                        (collect slot-value-group))))))))

(def (layered-function e) make-standard-object-detail-filter-class (component class prototype)
  (:method ((component standard-object-detail-filter) (class standard-class) (prototype standard-object))
    (localized-class-name class)))

(def (layered-function e) collect-standard-object-detail-filter-slot-groups (component class prototype slots)
  (:method ((component standard-object-detail-filter) (class standard-class) (prototype standard-object) (slots list))
    (list slots)))

(def (layered-function e) collect-standard-object-detail-filter-slots (component class prototype)
  (:method ((component standard-object-detail-filter) (class standard-class) (prototype standard-object))
    (class-slots class)))

(def render standard-object-detail-filter ()
  (bind (((:read-only-slots class-selector class slot-value-groups id) -self-))
    <div (:id ,id)
         <div ,(standard-object-detail-filter.instance class)>
         ,(when class-selector
                <div "Narrow down to "
                     ,(render class-selector)
                     ,(render (command (icon refresh)
                                       (make-action
                                         (setf (outdated-p -self-) #t))))>)
         <div <h3 ,#"standard-object-detail-filter.slots">
              <table ,(map nil #'render slot-value-groups)>>>))

(defresources en
  (standard-object-detail-filter.instance (class)
    <span "Searching for instances of" ,(render class)>)
  (standard-object-detail-filter.slots "Slots"))

(defresources hu
  (standard-object-detail-filter.instance (class)
    <span ,(render class) " keresése">)
  (standard-object-detail-filter.slots "Tulajdonságok"))

;;;;;;
;;; Standard object slot value group filter

(def component standard-object-slot-value-group-filter (standard-object-slot-value-group-component filter-component)
  ())

(def method refresh-component ((self standard-object-slot-value-group-filter))
  (with-slots (the-class slots slot-values) self
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value = (find slot slot-values :key #'component-value-of))
                (if slot-value
                    (setf (component-value-of slot-value) slot)
                    (setf slot-value (make-instance 'standard-object-slot-value-filter :the-class the-class :slot slot)))
                (collect slot-value)))))

(def render standard-object-slot-value-group-filter ()
  (bind (((:read-only-slots slot-values id) -self-))
    (if slot-values
        (progn
          <thead <tr <th (:colspan 4) ,#"standard-object-slot-value-group.column.name">
                     <th ,#"standard-object-slot-value-group.column.value">>>
          <tbody ,(map nil #'render slot-values)>)
        <span (:id ,id) ,#"there-are-none">)))

;;;;;;
;;; Standard object slot value filter

(def component standard-object-slot-value-filter (standard-object-slot-value-component filter-component)
  ()
  (:documentation "Filter for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def method refresh-component ((self standard-object-slot-value-filter))
  (with-slots (slot label value) self
    (setf label (label (localized-slot-name slot))
          value (make-place-filter (slot-type slot) :name (slot-definition-name slot)))))

(def render standard-object-slot-value-filter ()
  (bind (((:read-only-slots label value id) -self-))
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
        <td (:class "slot-value-label")
            ,(render label)>
        ;; NOTE: the value component is resposible to render the cells
        ,(render value)>))

;;;;;;
;;; Standard object place filter

(def component standard-object-place-filter (place-filter)
  ())

(def method make-place-component-content ((self standard-object-place-filter))
  (make-inspector (the-type-of self) :default-component-type 'reference-component))

(def method make-place-component-command-bar ((self standard-object-place-filter))
  (make-instance 'command-bar-component :commands (list (make-set-place-to-nil-command self)
                                                        (make-set-place-to-find-instance-command self))))

(def method collect-possible-filter-predicates ((self standard-object-place-filter))
  '(=))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command (filter result)
  (make-replace-and-push-back-command result (delay (with-restored-component-environment filter
                                                      (funcall (result-component-factory-of filter) filter
                                                               (execute-filter-instances filter (the-class-of filter)))))
                                      (list :icon (icon filter))
                                      (list :icon (icon back))))

(def (layered-function e) make-standard-object-filter-result-inspector (filter result)
  (:method ((filter standard-object-filter) (instances list))
    (make-viewer instances :type `(list ,(class-name (the-class-of filter)))))

  (:method :around ((filter standard-object-filter) (instances list))
    (prog1-bind component
        (call-next-method)
      (unless instances
        (add-user-warning component #"no-matches-were-found")))))

(def (layered-function e) execute-filter-instances (component class)
  (:method ((component standard-object-filter) (class standard-class))
    (execute-filter-instances (content-of component) class))

  #+sbcl
  (:method ((component standard-object-detail-filter) (class standard-class))
    (bind ((slot-values (mappend #'slot-values-of (slot-value-groups-of component)))
           (predicates (iter (for slot-value :in slot-values)
                             (for predicate = (bind ((slot-name (slot-definition-name (slot-of slot-value)))
                                                     (place-filter (value-of slot-value))
                                                     (value-component (content-of place-filter))
                                                     (predicate-function (bind ((function (fdefinition (predicate-function place-filter class (selected-predicate-of place-filter)))))
                                                                           (if (negated-p place-filter)
                                                                               (complement function)
                                                                               function))))
                                                (when (use-in-filter-p place-filter)
                                                  (lambda (instance)
                                                    (bind ((instance-class (class-of instance))
                                                           (slot (find-slot instance-class slot-name)))
                                                      (and (slot-boundp-using-class instance-class instance slot)
                                                           (funcall predicate-function
                                                                    (slot-value-using-class instance-class instance slot)
                                                                    (component-value-of value-component))))))))
                             (when predicate
                               (collect predicate)))))
      (prog1-bind instances nil
        (sb-vm::map-allocated-objects
         (lambda (instance type size)
           (declare (ignore type size))
           (bind ((instance-class (class-of instance)))
             (when (and (typep instance class)
                        (not (eq instance (class-prototype instance-class)))
                        (every (lambda (predicate)
                                 (funcall predicate instance))
                               predicates))
               (push instance instances))))
         :dynamic)))))

(defresources en
  (no-matches-were-found "No matching objects were found"))

(defresources hu
  (no-matches-were-found "Nincs a keresésnek megfelelő objektum"))
