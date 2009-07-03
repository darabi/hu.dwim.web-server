;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object inspector

(def (component e) standard-object/inspector (standard-object/mixin
                                              inspector/abstract
                                              editable/mixin
                                              exportable/abstract
                                              alternator/basic
                                              initargs/mixin
                                              layer-context-capturing/mixin)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT in various alternative views."))

(def (macro e) standard-object/inspector (instance)
  `(make-instance 'standard-object/inspector :instance ,instance))

(def layered-method make-title ((self standard-object/inspector))
  (title (standard-object-inspector.title (localized-class-name (component-dispatch-class self)))))

(def layered-methods make-alternatives
  (:method ((component standard-object-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-detail-inspector :instance instance)
          (delay-alternative-reference-component 'standard-object-inspector-reference instance)))

  (:method ((component standard-object-inspector) (class built-in-class) (prototype null) (instance null))
    (list (delay-alternative-component-with-initargs 'null-component))))

(def (layered-function e) make-delete-instance-command (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (command (:visible (delay (not (edited? component)))
              :js (lambda (href)
                    (render-dojo-dialog (dialog-id :title #"delete-instance.dialog.title")
                      <div
                       ,(bind ((instance (instance-of component)))
                              (funcall-resource-function 'delete-instance.dialog.body :class (class-of instance) :instance instance))
                       ,(render-dojo-dialog/buttons
                         (#"Icon-label.cancel" `js-inline(.hide (dijit.byId ,dialog-id)))
                         (#"Icon-label.delete" `js-inline(wui.io.action ,href :ajax #f)))>)))
      (icon delete)
      (make-action
        (bind ((instance (instance-of component)))
          (delete-instance component (class-of instance) instance))))))

(def (layered-function e) delete-instance (component class instance)
  (:method ((component standard-object-inspector) (class standard-class) (instance standard-object))
    (setf (component-value-of component) nil)))

(def layered-method render-onclick-handler ((self standard-object-inspector) (button (eql :left)))
  #+nil ;; TODO: this prevents clicking into edit fields in edit mode, because collapses the component
  (when-bind collapse-command (find-command self 'collapse)
    (render-command-onclick-handler collapse-command (id-of self))))

;;;;;;
;;; Standard object detail inspector

(def (component e) standard-object-detail-inspector (standard-object/mixin
                                                    standard-object-detail-component
                                                    inspector/abstract
                                                    editable/mixin)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT in detail."))

(def refresh-component standard-object-detail-inspector
  (bind (((:slots instance slot-value-groups) -self-)
         (the-class (when instance (class-of instance))))
    ;; TODO: factor this out into a base class throughout this directory
    (if instance
        (bind ((slots (collect-standard-object-detail-inspector-slots -self- the-class instance))
               (slot-groups (collect-standard-object-detail-slot-groups -self- the-class instance slots)))
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
        (setf slot-value-groups nil))))

(def (layered-function e) collect-standard-object-detail-inspector-slots (component class instance)
  (:method ((component standard-object-detail-inspector) (class standard-class) (instance standard-object))
    (class-slots class)))

(def render-xhtml standard-object-detail-inspector
  (bind (((:read-only-slots slot-value-groups id) -self-))
    <div (:id ,id :class "standard-object")
         <table (:class "slot-table") ,(foreach #'render-component slot-value-groups)>>
    (render-onclick-handler (parent-component-of -self-) :left)))

;;;;;;
;;; Standard object slot value group inspector

(def (component e) standard-object-slot-value-group-inspector (standard-object-slot-value-group-component
                                                              standard-object/mixin
                                                              inspector/abstract
                                                              editable/mixin)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and a list of STANDARD-SLOT-DEFINITION instances."))

(def refresh-component standard-object-slot-value-group-inspector
  (bind (((:slots instance slots slot-values) -self-))
    (if instance
        (setf slot-values
              (iter (for slot :in slots)
                    (for slot-value-component = (find-slot-value-component slot slot-values))
                    (if slot-value-component
                        (setf (component-value-of slot-value-component) slot
                              (instance-of slot-value-component) instance)
                        (setf slot-value-component (make-standard-object-slot-value-inspector -self- (class-of instance) instance slot)))
                    (collect slot-value-component)))
        (setf slot-values nil))))

(def (layered-function e) make-standard-object-slot-value-inspector (component class instance slot)
  (:method ((component standard-object-slot-value-group-inspector) (class standard-class) (instance standard-object) (slot standard-effective-slot-definition))
    (make-instance 'standard-object-slot-value-inspector :the-class class :instance instance :slot slot)))

;;;;;;
;;; Standard object slot value inspector

(def (component e) standard-object-slot-value-inspector (standard-object-slot-value/inspector
                                                        standard-object/mixin
                                                        inspector/abstract
                                                        editable/mixin)
  ()
  (:documentation "Inspector for an instance of STANDARD-OBJECT and an instance of STANDARD-SLOT-DEFINITION."))

(def refresh-component standard-object-slot-value-inspector
  (bind (((:slots instance slot label value) -self-))
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

(def (component e) standard-object-place-inspector (place-inspector)
  ()
  (:documentation "Inspector for a place of an instance of STANDARD-OBJECT and unit types."))

(def method make-place-component-command-bar ((self standard-object-place-inspector))
  (make-instance 'command-bar/basic :commands (list (make-revert-place-command self)
                                                    (make-set-place-to-nil-command self)
                                                    (make-set-place-to-find-instance-command self)
                                                    (make-set-place-to-new-instance-command self))))
