;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def (component e) standard-object-filter (standard-class/mixin
                                          filter/abstract
                                          alternator/basic
                                          initargs/mixin
                                          layer-context-capturing/mixin)
  ((result (empty) :type component)
   (result-component-factory #'make-standard-object-filter-result-inspector :type function))
  (:documentation "Filter for instances of STANDARD-OBJECT in various alternative views."))

;; TODO: is this really what we want?
(def render-component standard-object-filter
  <div ,(call-next-method)
       ,(render-component (result-of -self-))>)

(def (macro e) standard-object-filter (the-class)
  `(make-instance 'standard-object-filter :the-class ,the-class))

(def layered-method make-title ((self standard-object-filter))
  (title (standard-object-filter.title (localized-class-name (the-class-of self)))))

(def layered-method make-alternatives ((component standard-object-filter) (class standard-class) (prototype standard-object) value)
  (list (delay-alternative-component-with-initargs 'standard-object-detail-filter :the-class class)
        (delay-alternative-reference-component 'standard-object-filter-reference class)))

(def layered-method make-context-menu-items ((component standard-object-filter) (class standard-class) (prototype standard-object) (instance standard-object))
  (optional-list* (make-filter-instances-command component (delay (result-of component))) (call-next-method)))

(def layered-method make-command-bar-commands ((component standard-object-filter) (class standard-class) (prototype standard-object) (instance standard-object))
  (optional-list* (make-filter-instances-command component (delay (result-of component))) (call-next-method)))

;;;;;;
;;; Standard object detail filter

(def (component e) standard-object-detail-filter (standard-object-detail-component
                                                 standard-class/mixin
                                                 filter/abstract)
  ((class-selector nil :type component)
   (ordering-specifier nil :type component)))

(def (macro e) standard-object-detail-filter (class)
  `(make-instance 'standard-object-detail-filter :the-class ,class))

(def (layered-function e) collect-standard-object-detail-filter-classes (component class prototype)
  (:method ((component standard-object-detail-filter) (class standard-class) (prototype standard-object))
    (list* class (subclasses class))))

(def refresh-component standard-object-detail-filter
  (bind (((:slots class-selector ordering-specifier the-class slot-value-groups command-bar) -self-)
         (selectable-classes (collect-standard-object-detail-filter-classes -self- the-class (class-prototype the-class))))
    (if (length= selectable-classes 1)
        (setf class-selector nil)
        (if class-selector
            (setf (possible-values-of class-selector) selectable-classes)
            (setf class-selector (make-class-selector selectable-classes))))
    (bind ((selected-class (if class-selector
                               (component-value-of class-selector)
                               (first selectable-classes)))
           (prototype (class-prototype selected-class))
           (slots (collect-standard-object-detail-filter-slots -self- selected-class prototype)))
      (setf ordering-specifier (when slots (make-slot-selector slots))
            slot-value-groups (bind ((slot-groups (collect-standard-object-detail-slot-groups -self- selected-class prototype slots)))
                                (iter (for (name . slot-group) :in slot-groups)
                                      (when slot-group
                                        (for slot-value-group = (find-slot-value-group-component slot-group slot-value-groups))
                                        (if slot-value-group
                                            (setf (component-value-of slot-value-group) slot-group
                                                  (the-class-of slot-value-group) selected-class
                                                  (name-of slot-value-group) name)
                                            (setf slot-value-group (make-instance 'standard-object-slot-value-group-filter
                                                                                  :slots slot-group
                                                                                  :the-class selected-class
                                                                                  :name name)))
                                        (collect slot-value-group))))))))

(def (layered-function e) collect-standard-object-detail-filter-slots (component class prototype)
  (:method ((component standard-object-detail-filter) (class standard-class) (prototype standard-object))
    (class-slots class)))

(def render-xhtml standard-object-detail-filter
  (bind (((:read-only-slots class-selector ordering-specifier slot-value-groups id) -self-))
    <div (:id ,id)
         <table (:class "slot-table")
           ,(when (or class-selector ordering-specifier)
                  <tbody ,(when class-selector
                                <tr <td ,#"standard-object-detail-filter.class-selector-label">
                                    <td (:colspan 3)>
                                    <td ,(render-component class-selector)>>)
                         ,(when ordering-specifier
                                <tr <td ,#"standard-object-detail-filter.ordering-specifier-label">
                                    <td (:colspan 3)>
                                    <td ,(render-component ordering-specifier)>>) >)
           ,(foreach #'render-component slot-value-groups)>>))

;;;;;;
;;; Standard object slot value group filter

(def (component e) standard-object-slot-value-group-filter (standard-object-slot-value-group-component filter/abstract)
  ())

(def refresh-component standard-object-slot-value-group-filter
  (bind (((:slots the-class slots slot-values) -self-))
    (setf slot-values
          (iter (for slot :in slots)
                (for slot-value = (find-slot-value-component slot slot-values))
                (if slot-value
                    (setf (component-value-of slot-value) slot)
                    (setf slot-value (make-instance 'standard-object-slot-value-filter :the-class the-class :slot slot)))
                (collect slot-value)))))

(def method standard-object-slot-value-group-column-count ((self standard-object-slot-value-group-filter))
  5)

;;;;;;
;;; Standard object slot value filter

(def (component e) standard-object-slot-value-filter (standard-object-slot-value/inspector filter/abstract)
  ()
  (:documentation "Filter for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def refresh-component standard-object-slot-value-filter
  (bind (((:slots slot label value) -self-)
         (type (slot-type slot))
         (name (slot-definition-name slot)))
    (setf label (localized-slot-name slot))
    (unless (and value
                 (type= (the-type-of value) type)
                 (eq (name-of value) name))
      (setf value (make-place-filter type :name name)))))

(def render-xhtml standard-object-slot-value-filter
  (bind (((:read-only-slots label value id) -self-))
    <tr (:id ,id :class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
        <td (:class "slot-value-label")
            ,(render-component label)>
        ;; NOTE: the value component is resposible to render the cells
        ,(render-component value)>))

;;;;;;
;;; Standard object place filter

(def (component e) standard-object-place-filter (place-filter)
  ())

(def method make-place-component-content ((self standard-object-place-filter))
  (make-inspector (the-type-of self) :initial-alternative-type 'reference-component))

(def method make-place-component-command-bar ((self standard-object-place-filter))
  (make-instance 'command-bar/basic :commands (list (make-set-place-to-nil-command self)
                                                    (make-set-place-to-find-instance-command self))))

(def method collect-possible-filter-predicates ((self standard-object-place-filter))
  '(=))

;;;;;;
;;; Filter

(def (layered-function e) make-filter-instances-command (component result)
  (:method ((component filter/abstract) result)
    (make-replace-and-push-back-command result (delay (with-restored-component-environment component
                                                        (funcall (result-component-factory-of component) component
                                                                 (filter-instances component (the-class-of component)))))
                                        (list :content (icon filter) :default #t)
                                        (list :content (icon back)))))

(def (layered-function e) make-standard-object-filter-result-inspector (filter result)
  (:method ((filter standard-object-filter) (instances list))
    (make-viewer instances :type `(list ,(class-name (the-class-of filter)))))

  (:method :around ((filter standard-object-filter) (instances list))
    (prog1-bind component
        (call-next-method)
      (unless instances
        (add-component-warning-message component #"no-matches-were-found")))))

(def (layered-function e) filter-instances (component class)
  (:method ((component standard-object-filter) (class standard-class))
    (filter-instances (content-of component) class))

  #+sbcl
  (:method ((component standard-object-detail-filter) (class standard-class))
    (bind ((slot-values (mappend #'slot-values-of (slot-value-groups-of component)))
           (predicates (iter (for slot-value :in slot-values)
                             (for predicate = (bind ((slot-name (slot-definition-name (slot-of slot-value)))
                                                     (place-filter (value-of slot-value))
                                                     (value-component (content-of place-filter))
                                                     (predicate-function (bind ((function (ensure-function (predicate-function place-filter class (selected-predicate-of place-filter)))))
                                                                           (if (negated-p place-filter)
                                                                               (complement function)
                                                                               function))))
                                                (when (use-in-filter? place-filter)
                                                  (lambda (instance)
                                                    (bind ((instance-class (class-of instance))
                                                           (slot (find-slot instance-class slot-name)))
                                                      (and (slot-boundp-using-class instance-class instance slot)
                                                           (funcall predicate-function
                                                                    (slot-value-using-class instance-class instance slot)
                                                                    (component-value-of value-component))))))))
                             (when predicate
                               (collect predicate)))))
      (bind ((instances ()))
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
         :dynamic
         t)
        instances))))
