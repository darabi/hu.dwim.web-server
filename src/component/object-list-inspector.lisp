;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list

(def component standard-object-list-inspector (abstract-standard-object-list-component
                                               abstract-standard-class-component
                                               inspector-component
                                               editable-component
                                               alternator-component
                                               user-message-collector-component-mixin
                                               remote-identity-component-mixin
                                               initargs-component-mixin
                                               layered-component-mixin)
  ((the-class (find-class 'standard-object)))
  (:default-initargs :alternatives-factory #'make-standard-object-list-inspector-alternatives)
  (:documentation "Inspector for a list of STANDARD-OBJECT instances in various alternative views."))

(def (macro e) standard-object-list-inspector (instances)
  `(make-instance 'standard-object-inspector :instances ,instances))

(def method refresh-component ((self standard-object-list-inspector))
  (with-slots (instances the-class default-component-type alternatives content command-bar) self
    (if alternatives
        (setf (component-value-for-alternatives self) instances)
        (setf alternatives (funcall (alternatives-factory-of self) self the-class (class-prototype the-class) instances)))
    (if content
        (setf (component-value-of content) instances)
        (setf content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives))))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (append (list (make-open-in-new-frame-command self)
                                                                 (make-top-command self)
                                                                 (make-refresh-command self))
                                                           (make-standard-object-list-inspector-commands self the-class (class-prototype the-class)))))))

(def render standard-object-list-inspector ()
  (with-slots (id) -self-
    <div (:id ,id :class "standard-object-list")
         ,(render-user-messages -self-)
         ,(call-next-method)>))

(def (layered-function e) make-standard-object-list-inspector-alternatives (component class prototype instances)
  (:method ((component standard-object-list-inspector) (class standard-class) (prototype standard-object) (instances list))
    (list (delay-alternative-component-with-initargs 'standard-object-list-table-inspector :the-class class :instances instances)
          (delay-alternative-component-with-initargs 'standard-object-list-list-inspector :the-class class :instances instances)
          (delay-alternative-reference-component 'standard-object-list-reference instances))))

(def (layered-function e) make-standard-object-list-inspector-commands (component class prototype)
  (:method ((component standard-object-list-inspector) (class standard-class) (prototype standard-object))
    (make-editing-commands component))

  (:method ((component standard-object-list-inspector) (class prc::persistent-class) (prototype prc::persistent-object))
    (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
      (make-editing-commands component))))

;;;;;;
;;; Standard object list list

(def component standard-object-list-list-inspector (abstract-standard-object-list-component
                                                    abstract-standard-class-component
                                                    inspector-component
                                                    list-component
                                                    editable-component)
  ((component-factory
    #'make-standard-object-list-component
    :type (or symbol function))))

(def method refresh-component ((self standard-object-list-list-inspector))
  (with-slots (instances the-class components) self
    (setf components
          (iter (for instance :in instances)
                (for component = (find instance components :key #'component-value-of))
                (if component
                    (setf (component-value-of component) instance)
                    (setf component (funcall (component-factory-of self) self (class-of instance) instance)))
                (collect component)))))

(def (generic e) make-standard-object-list-component (component class instance)
  (:method ((component standard-object-list-list-inspector) (class standard-class) (instance standard-object))
    (make-viewer-component instance :default-component-type 'reference-component)))

;;;;;;
;;; Standard object table

(def component standard-object-list-table-inspector (abstract-standard-object-list-component
                                                     abstract-standard-class-component
                                                     inspector-component
                                                     table-component
                                                     editable-component)
  ())

(def method refresh-component ((self standard-object-list-table-inspector))
  (with-slots (instances the-class columns rows) self
    (setf columns (make-standard-object-list-table-inspector-columns self))
    (setf rows (iter (for instance :in instances)
                     (for row = (find instance rows :key #'component-value-of))
                     (if row
                         (setf (component-value-of row) instance)
                         (setf row (make-instance 'standard-object-row-inspector :instance instance)))
                     (collect row)))))

(def (function e) make-standard-object-list-table-type-column ()
  (make-instance 'column-component
                 :content (label #"object-list-table.column.type")
                 :cell-factory (lambda (row-component)
                                 (make-instance 'cell-component :content (make-instance 'standard-class-component
                                                                                        :the-class (class-of (instance-of row-component))
                                                                                        :default-component-type 'reference-component)))))

(def (function e) make-standard-object-list-table-command-bar-column ()
  (make-instance 'column-component
                    :content (label #"object-list-table.column.commands")
                    :visible (delay (not (layer-active-p 'passive-components-layer)))
                    :cell-factory (lambda (row-component)
                                    (make-instance 'cell-component :content (command-bar-of row-component)))))

(def (generic e) make-standard-object-list-table-inspector-columns (component)
  (:method ((self standard-object-list-table-inspector))
    (append (optional-list (make-standard-object-list-table-command-bar-column)
                           (when-bind the-class (the-class-of self)
                             (when (closer-mop:class-direct-subclasses the-class)
                               (make-standard-object-list-table-type-column))))
            (make-standard-object-list-table-inspector-slot-columns self))))

(def (generic e) make-standard-object-list-table-inspector-slot-columns (component)
  (:method ((self standard-object-list-table-inspector))
    (with-slots (instances the-class command-bar columns rows) self
      (bind ((slot-name->slot-map (list)))
        ;; KLUDGE: TODO: this register mapping is wrong, maps slot-names to randomly choosen effective-slots, must be forbidden
        (flet ((register-slot (slot)
                 (bind ((slot-name (slot-definition-name slot)))
                   (unless (member slot-name slot-name->slot-map :test #'eq :key #'car)
                     (push (cons slot-name slot) slot-name->slot-map)))))
          (when the-class
            (mapc #'register-slot (collect-standard-object-list-table-inspector-slots self the-class (class-prototype the-class))))
          (dolist (instance instances)
            (mapc #'register-slot (collect-standard-object-list-table-inspector-slots self (class-of instance) instance))))
        (mapcar (lambda (slot-name->slot)
                  (make-instance 'column-component
                                 :content (label (localized-slot-name (cdr slot-name->slot)))
                                 :cell-factory (lambda (row-component)
                                                 (bind ((slot (find-slot (class-of (instance-of row-component)) (car slot-name->slot))))
                                                   (if slot
                                                       (make-instance 'standard-object-slot-value-cell-component :instance (instance-of row-component) :slot slot)
                                                       (make-instance 'cell-component :content (label "N/A")))))))
                (nreverse slot-name->slot-map))))))

(def (generic e) collect-standard-object-list-table-inspector-slots (component class instance)
  (:method ((component standard-object-list-table-inspector) (class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((component standard-object-list-table-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-list-table-inspector) (class dmm::entity) (instance prc::persistent-object))
    (filter-if #'dmm::primary-p (call-next-method))))

(defresources hu
  (object-list-table.column.commands "Műveletek")
  (object-list-table.column.type "Típus"))

(defresources en
  (object-list-table.column.commands "Commands")
  (object-list-table.column.type "Type"))

;;;;;;
;;; Standard object row

(def component standard-object-row-inspector (abstract-standard-object-component
                                              inspector-component
                                              editable-component
                                              row-component
                                              user-message-collector-component-mixin
                                              remote-identity-component-mixin)
  ((command-bar nil :type component)))

(def method refresh-component ((self standard-object-row-inspector))
  (with-slots (instance command-bar cells) self
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-object-row-inspector-commands self (class-of instance) instance))
              cells (mapcar (lambda (column)
                              (funcall (cell-factory-of column) self))
                            (columns-of (find-ancestor-component-with-type self 'standard-object-list-table-inspector))))
        (setf command-bar nil
              cells nil))))

(def layered-method render-table-row ((table standard-object-list-table-inspector) (row standard-object-row-inspector))
  (when (messages-of row)
    (render-entire-row table
                       (lambda ()
                         (render-user-messages row))))
  (call-next-method))

(def (layered-function e) make-standard-object-row-inspector-commands (component class instance)
  (:method ((component standard-object-row-inspector) (class standard-class) (instance standard-object))
    (append (make-editing-commands component)
            (list (make-expand-row-command component instance))))

  (:method ((component standard-object-row-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
              (make-editing-commands component))
            (list (make-expand-row-command component instance)))))

(def function make-expand-row-command (component instance)
  (make-replace-and-push-back-command component (delay (make-instance '(editable-component entire-row-component) :content (make-viewer-component instance :default-component-type 'detail-component)))
                                      (list :icon (icon expand)
                                            :visible (delay (not (has-edited-descendant-component-p component))))
                                      (list :icon (icon collapse))))

;;;;;;
;;; Standard object slot value cell

(def component standard-object-slot-value-cell-component (abstract-standard-object-slot-value-component
                                                          cell-component
                                                          editable-component)
  ())

(def method refresh-component ((component standard-object-slot-value-cell-component))
  (with-slots (instance slot content) component
    (if slot
        (setf content (make-instance 'place-component :place (make-slot-value-place instance slot)))
        (setf content nil))))
