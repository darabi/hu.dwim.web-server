;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object inspector

(def component standard-object-inspector (abstract-standard-object-component
                                          inspector-component
                                          editable-component
                                          exportable-component
                                          alternator-component
                                          user-message-collector-component-mixin
                                          remote-identity-component-mixin
                                          initargs-component-mixin
                                          layer-context-capturing-component-mixin
                                          recursion-point-component)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-inspector-alternatives)
  (:documentation "Inspector for an instance of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object-inspector (instance)
  `(make-instance 'standard-object-inspector :instance ,instance))

(def method refresh-component ((self standard-object-inspector))
  (with-slots (instance default-component-type alternatives content command-bar) self
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) instance)
              (setf alternatives (funcall (alternatives-factory-of self) self (class-of instance) instance)))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) instance)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-alternator-command-bar self alternatives
                                                         (make-standard-commands self (class-of instance) instance))))
        (setf alternatives (list (delay-alternative-component-with-initargs 'null-component))
              content (find-default-alternative-component alternatives)))))

(def render standard-object-inspector ()
  (bind (((:read-only-slots content id) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (call-next-method)))
      (if (typep content '(or reference-component primitive-component))
          <span (:id ,id :class "standard-object-inspector")
            ,(body)>
          (progn
            <div (:id ,id :class "standard-object-inspector")
              ,(body)>
            `js(wui.setup-widget "standard-object-inspector" ,id))))))

(def (layered-function e) make-standard-object-inspector-alternatives (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-inspector :instance instance)
          (delay-alternative-reference-component 'standard-object-inspector-reference instance))))

(def (function e) make-delete-instance-command (self)
  (command (icon delete)
           (make-action
             (bind ((instance (instance-of self)))
               (execute-delete-instance self (class-of instance) instance)))
           :js (lambda (href)
                 (render-dojo-dialog (dialog-id :title #"delete-instance.dialog.title")
                   <div
                    ,(bind ((instance (instance-of self)))
                       (funcall-resource-function 'delete-instance.dialog.body :class (class-of instance) :instance instance))
                    ,(render-dojo-dialog/buttons
                      (#"Icon-label.cancel" `js-inline(.hide (dijit.byId ,dialog-id)))
                      (#"Icon-label.delete" `js-inline(wui.io.action ,href :ajax #f)))>))
           :visible (delay (not (edited-p self)))))

(def (layered-function e) execute-delete-instance (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (setf (component-value-of component) nil)))

(def layered-method render-onclick-handler ((self standard-object-inspector))
  #+nil ;; TODO: this prevents clicking into edit fields in edit mode, because collapses the component
  (when-bind collapse-command (find-command-bar-command (command-bar-of self) 'collapse)
    (render-command-onclick-handler collapse-command (id-of self))))

;;;;;;
;;; Standard object detail inspector

(def component standard-object-detail-inspector (abstract-standard-object-component
                                                 standard-object-detail-component
                                                 inspector-component
                                                 editable-component
                                                 title-component-mixin)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT in detail."))

(def method refresh-component ((self standard-object-detail-inspector))
  (with-slots (instance class slot-value-groups) self
    (bind ((the-class (when instance (class-of instance))))
      ;; TODO: factor this out into a base class throughout this directory
      (if the-class
          (if class
              (when (typep the-class 'abstract-standard-class-component)
                (setf (the-class-of class) the-class))
              (setf class (make-class-presentation self the-class (class-prototype the-class))))
          (setf class nil))
      (if instance
          (bind ((slots (collect-standard-object-detail-inspector-slots self the-class instance))
                 (slot-groups (collect-standard-object-detail-slot-groups self the-class instance slots)))
            (setf slot-value-groups
                  (iter (for (name . slot-group) :in slot-groups)
                        (when slot-group
                          (bind ((slot-value-group (find-slot-value-group-component slot-group slot-value-groups)))
                            (if slot-value-group
                                (setf (component-value-of slot-value-group) slot-group
                                      (instance-of slot-value-group) instance
                                      (the-class-of slot-value-group) the-class
                                      (name-of slot-value-group) name)
                                (setf slot-value-group (make-instance 'standard-object-slot-value-group-inspector
                                                                      :instance instance
                                                                      :slots slot-group
                                                                      :name name)))
                            (collect slot-value-group))))))
          (setf slot-value-groups nil)))))

(def (layered-function e) collect-standard-object-detail-inspector-slots (component class instance)
  (:method ((component standard-object-detail-inspector) (class standard-class) (instance standard-object))
    (class-slots class)))

(def render standard-object-detail-inspector ()
  (bind (((:read-only-slots slot-value-groups id) -self-))
    <div (:id ,id :class "standard-object")
         ,(render-title -self-)
         <table (:class "slot-table") ,(foreach #'render slot-value-groups)>>
    (render-onclick-handler (parent-component-of -self-))))

(def layered-method render-title ((self standard-object-detail-inspector))
  (standard-object-detail-inspector.title (slot-value self 'class)))


;;;;;;
;;; Standard object slot value group inspector

(def component standard-object-slot-value-group-inspector (standard-object-slot-value-group-component
                                                           abstract-standard-object-component
                                                           inspector-component
                                                           editable-component)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITION instances."))

(def method refresh-component ((self standard-object-slot-value-group-inspector))
  (with-slots (instance slots slot-values) self
    (if instance
        (setf slot-values
              (iter (for slot :in slots)
                    (for slot-value-component = (find-slot-value-component slot slot-values))
                    (if slot-value-component
                        (setf (component-value-of slot-value-component) slot
                              (instance-of slot-value-component) instance)
                        (setf slot-value-component (make-standard-object-slot-value-inspector self (class-of instance) instance slot)))
                    (collect slot-value-component)))
        (setf slot-values nil))))

(def (layered-function e) make-standard-object-slot-value-inspector (component class instance slot)
  (:method ((component standard-object-slot-value-group-inspector) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-inspector :the-class class :instance instance :slot slot)))

;;;;;;
;;; Standard object slot value inspector

(def component standard-object-slot-value-inspector (standard-object-slot-value-component
                                                     abstract-standard-object-component
                                                     inspector-component
                                                     editable-component)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def method refresh-component ((self standard-object-slot-value-inspector))
  (with-slots (instance slot label value) self
    (if slot
        (if (typep label 'component)
            (setf (component-value-of label) (localized-slot-name slot))
            (setf label (localized-slot-name slot)))
        (setf label nil))
    (if instance
        (if value
            (setf (place-of value) (make-slot-value-place instance slot))
            (setf value (make-standard-object-slot-value-place-inspector instance slot)))
        (setf value nil))))

;;;;;;
;;; Standard object place inspector

(def component standard-object-place-inspector (place-inspector)
  ()
  (:documentation "Inspector for a place of an instance of STANDARD-OBJECT and unit types."))

(def method make-place-component-command-bar ((self standard-object-place-inspector))
  (make-instance 'command-bar-component :commands (list (make-revert-place-command self)
                                                        (make-set-place-to-nil-command self)
                                                        (make-set-place-to-find-instance-command self)
                                                        (make-set-place-to-new-instance-command self))))
