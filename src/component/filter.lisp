;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter

(def component standard-object-filter-component ()
  ((the-class)
   (class :accessor nil :type component)
   (slot-values :type component)
   (result :type component)
   (command-bar :type component)))

(def constructor standard-object-filter-component ()
  (with-slots (the-class class slot-values result command-bar) self
    (setf class (make-viewer-component the-class :default-component-type 'reference-component)
          slot-values (make-instance 'standard-object-slot-value-group-component
                                     :instance nil
                                     :slot-values (mapcar (lambda (slot)
                                                            (make-instance 'standard-object-slot-value-filter-component :the-class the-class :slot slot))
                                                          (class-slots the-class)))
          result (make-instance 'empty-component)
          command-bar (make-instance 'command-bar-component :commands (list (make-top-command-component self)
                                                                            (make-filter-instances-command-component self))))))

(def render standard-object-filter-component ()
  (with-slots (the-class class slots-values slot-values result command-bar) self
    <div
      <span "Filter instances of " ,(render class)>
      <div
        <h3 "Slots">
        ,(render slot-values)
        ,(render command-bar)
        ,(render result)>>))

;;;;;;
;;; Standard object slot value filter

(def component standard-object-slot-value-filter-component ()
  ((the-class)
   (slot)
   (slot-name)
   (label nil :type component)
   (value nil :type component)))

(def constructor standard-object-slot-value-filter-component ()
  (with-slots (slot slot-name label value) self
    (setf slot-name (slot-definition-name slot)
          label (make-instance 'string-component :component-value (full-symbol-name slot-name))
          value (make-filter-component (slot-definition-type slot)))))

(def render standard-object-slot-value-filter-component ()
  (with-slots (label value) self
    <tr
      <td ,(render label)>
      <td ,(render value)>>))

;;;;;;
;;; Filter

(def (function e) make-filter-instances-command-component (component)
  (make-replace-and-push-back-command-component (result-of component) (delay (make-viewer-component (execute-filter component (the-class-of component))))
                                                (list :icon (make-icon-component 'filter :label "Filter" :tooltip "Execute filter"))
                                                (list :icon (make-icon-component 'back :label "Back" :tooltip "Back to filter"))))

(def generic execute-filter (component class)
  (:method ((component standard-object-filter-component) (class standard-class))
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
