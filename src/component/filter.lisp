;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def component standard-object-filter-component (alternator-component)
  ((the-class)
   (result :type component)))

(def constructor standard-object-filter-component ()
  (with-slots (the-class result default-component-type alternatives content command-bar) -self-
    (setf result (make-instance 'empty-component)
          alternatives (list (delay-alternative-component-type 'standard-object-filter-detail-component :the-class the-class)
                             (delay-alternative-component 'standard-object-filter-reference-component
                               (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-filter-reference-component :target the-class))))
          content (if default-component-type
                      (find-alternative-component alternatives default-component-type)
                      (find-default-alternative-component alternatives))
          command-bar (make-instance 'command-bar-component :commands (append (list (make-top-command-component -self-)
                                                                                    (make-filter-instances-command-component -self-))
                                                                              (make-alternative-command-components -self- alternatives))))))

;;;;;;
;;; Standard object filter detail

(def component standard-object-filter-detail-component ()
  ((the-class)
   (class :accessor nil :type component)
   (slot-value-group :type component)))

(def constructor standard-object-filter-detail-component ()
  (with-slots (the-class class slot-value-group result command-bar) -self-
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-value-group (make-instance 'standard-object-slot-value-group-filter-component
                                          :slots (class-slots the-class)
                                          :slot-values (mapcar (lambda (slot)
                                                                 (make-instance 'standard-object-slot-value-filter-component :the-class the-class :slot slot))
                                                               (class-slots the-class))))))

(def render standard-object-filter-detail-component ()
  (with-slots (the-class class slots-values slot-value-group result command-bar) -self-
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

(def component standard-object-slot-value-filter-component ()
  ((the-class)
   (slot)
   (slot-name)
   (negated #f :type boolean)
   (condition :equal :type (member (:equal :like :less :less-or-equal :greater :greater-or-equal :between)))
   (label nil :type component)
   (value nil :type component)))

(def constructor standard-object-slot-value-filter-component ()
  (with-slots (slot slot-name label negate condition value) -self-
    (setf slot-name (slot-definition-name slot)
          label (make-instance 'label-component :component-value (full-symbol-name slot-name))
          value (make-filter-component (slot-definition-type slot) :default-component-type 'reference-component))))

(def render standard-object-slot-value-filter-component ()
  (with-slots (label negated condition value) -self-
    <tr (:class ,(odd/even-class -self- (slot-values-of (parent-component-of -self-))))
      <td ,(render label)>
      <td ,(if negated
               (render-icon (find-icon 'negated) :render-label #f)
               (render-icon (find-icon 'ponated) :render-label #f))>
      <td ,(render-icon
            (find-icon
             (ecase condition
               (:equal 'equal)
               (:like 'like)))
            :render-label #f)>
      <td ,(render value)>>))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command-component (component)
  (make-replace-and-push-back-command-component (result-of component) (delay (make-viewer-component (execute-filter component (the-class-of component))))
                                                (list :icon (clone-icon 'filter))
                                                (list :icon (clone-icon 'back))))

(def generic execute-filter (component class)
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
         :dynamic)))))
