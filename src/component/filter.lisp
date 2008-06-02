;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def component standard-object-filter-component (alternator-component)
  ((the-class)))

(def constructor standard-object-filter-component ()
  (with-slots (the-class default-component-type alternatives content command-bar) -self-
    (setf alternatives (list (delay-alternative-component-type 'standard-object-filter-detail-component :the-class the-class)
                             (delay-alternative-component 'standard-object-filter-reference-component
                               (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-filter-reference-component :target the-class))))
          content (if default-component-type
                      (find-alternative-component alternatives default-component-type)
                      (find-default-alternative-component alternatives))
          command-bar (make-instance 'command-bar-component :commands (append (list (make-top-command-component -self-)
                                                                                    (make-filter-instances-command-component -self-))
                                                                              (make-alternative-command-components -self- alternatives))))))

(def function make-standard-object-filter-component (&rest args &key slots &allow-other-keys)
  (make-instance 'standard-object-filter-component))

;;;;;;
;;; Standard object filter detail

(def component standard-object-filter-detail-component ()
  ((the-class)
   (class :accessor nil :type component)
   (slot-value-group :type component)))

(def constructor standard-object-filter-detail-component ()
  (with-slots (the-class class slot-value-group command-bar) -self-
    (bind ((slots (standard-object-filter-detail-slots the-class)))
      (setf class (make-viewer-component the-class :default-component-type 'reference-component)
            slot-value-group (make-instance 'standard-object-slot-value-group-filter-component
                                            :slots slots
                                            :slot-values (mapcar (lambda (slot)
                                                                   (make-instance 'standard-object-slot-value-filter-component :the-class the-class :slot slot))
                                                                 slots))))))

(def generic standard-object-filter-detail-slots (class)
  (:method ((class standard-class))
    (class-slots class))

  (:method ((class prc::persistent-class))
    (iter (for slot :in (prc::persistent-effective-slots-of class))
          (if (dmm::authorize-operation 'dmm::filter-entity-property-operation :entity class :property slot)
              (collect slot)))))

(def render standard-object-filter-detail-component ()
  (with-slots (the-class class slots-values slot-value-group command-bar) -self-
    <div
      <span "Filter instances of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-value-group)>>))

;;;;;;
;;; Standard object slot value group

(def component standard-object-slot-value-group-filter-component ()
  ((slots nil)
   (slot-values nil :type components)))

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-group-filter-component))
     (with-slots (instance the-class slots slot-values) component
       (setf slot-values
             (iter (for slot :in slots)
                   (for slot-value-detail = (find slot slot-values :key #'component-value-of))
                   (if slot-value-detail
                       (setf (component-value-of slot-value-detail) instance
                             (slot-of slot-value-detail) slot)
                       (setf slot-value-detail (make-instance 'standard-object-slot-value-detail-component :the-class the-class :instance instance :slot slot)))
                   (collect slot-value-detail)))))

(def render standard-object-slot-value-group-filter-component ()
  (with-slots (slot-values) -self-
    (if slot-values
        <table
          <thead
            <tr
              <th "Name">
              <th>
              <th>
              <th "Value">>>
          <tbody ,@(mapcar #'render slot-values)>>
        <span "There are none">)))

;;;;;;
;;; Standard object slot value filter

;; TODO: all predicates
(def (constant :test 'equalp) +filter-predicates+ '(equal like #+nil (< <= > >= between)))

(def component standard-object-slot-value-filter-component ()
  ((the-class)
   (slot)
   (slot-name)
   (negated #f :type boolean)
   (negate-command :type component)
   (condition 'equal :type (member #.+filter-predicates+))
   (condition-command :type component)
   (label nil :type component)
   (value nil :type component)))

(def constructor standard-object-slot-value-filter-component ()
  (with-slots (slot slot-name negated negate-command condition condition-command label value) -self-
    (setf slot-name (slot-definition-name slot)
          label (make-instance 'label-component :component-value (full-symbol-name slot-name))
          negate-command (make-instance 'command-component
                                        :icon (make-negated/ponated-icon negated)
                                        :action (make-action
                                                  (setf negated (not negated))
                                                  (setf (icon-of negate-command) (make-negated/ponated-icon negated))))
          condition-command (make-instance 'command-component
                                           :icon (make-condition-icon condition)
                                           :action (make-action
                                                     (setf condition (elt +filter-predicates+
                                                                          (mod (1+ (position condition +filter-predicates+))
                                                                               (length +filter-predicates+))))
                                                     (setf (icon-of condition-command) (make-condition-icon condition))))
          value (make-filter-component (slot-definition-type slot) :default-component-type 'reference-component))))

(def function make-negated/ponated-icon (negated)
  (aprog1 (clone-icon (if negated 'negated 'ponated))
    (setf (label-of it) nil)))

(def function make-condition-icon (condition)
  (aprog1 (clone-icon
           (ecase condition
             (equal 'equal)
             (like 'like)))
    (setf (label-of it) nil)))

(def render standard-object-slot-value-filter-component ()
  (with-slots (label negate-command condition-command value) -self-
    <tr (:class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(render negate-command)>
      <td ,(render condition-command)>
      <td ,(render value)>>))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command-component (component)
  (make-replace-and-push-back-command-component component (delay (make-viewer-component (execute-filter component (the-class-of component))))
                                                (list :icon (clone-icon 'filter))
                                                (list :icon (clone-icon 'back))))

(def generic execute-filter (component class)
  (:method ((component standard-object-filter-component) class)
    (execute-filter (content-of component) class))

  (:method ((component standard-object-filter-detail-component) (class standard-class))
    (bind ((slot-values (slot-values-of (slot-values-of component)))
           (slot-names (mapcar #'slot-name-of slot-values))
           (values (mapcar (lambda (slot-value)
                             (bind ((component (value-of slot-value)))
                               (typecase component
                                 (atomic-component
                                  (component-value-of component)))))
                           slot-values)))
      (prog1-bind instances nil
        (sb-vm::map-allocated-objects
         (lambda (instance type size)
           (declare (ignore type size))
           (bind ((instance-class (class-of instance)))
             (when (and (typep instance class)
                        (not (eq instance (closer-mop:class-prototype instance-class)))
                        (every (lambda (slot-name value)
                                 (or (not value)
                                     (bind ((slot (find-slot instance-class slot-name)))
                                       (and slot
                                            (slot-boundp-using-class instance-class instance slot)
                                            (equal value (slot-value-using-class instance-class instance slot))))))
                               slot-names values))
               (push instance instances))))
         :dynamic))))

  (:method ((component standard-object-filter-detail-component) (class prc::persistent-class))
    (bind ((class-name (class-name (the-class-of component))))
      (prc::select (instance)
        (prc::from (instance prc::persistent-object))
        (prc::where (typep instance class-name))))))
