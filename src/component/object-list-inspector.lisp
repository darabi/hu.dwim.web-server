;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list inspector

(def component standard-object-list-inspector (abstract-standard-object-list-component
                                               abstract-standard-class-component
                                               inspector-component
                                               editable-component
                                               alternator-component
                                               user-message-collector-component-mixin
                                               remote-identity-component-mixin
                                               initargs-component-mixin
                                               layer-context-capturing-component-mixin)
  ((the-class (find-class 'standard-object)))
  (:default-initargs :alternatives-factory #'make-standard-object-list-inspector-alternatives)
  (:documentation "Inspector for a list of STANDARD-OBJECT instances in various alternative views."))

(def (macro e) standard-object-list-inspector (instances)
  `(make-instance 'standard-object-list-inspector :instances ,instances))

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
                                                           (make-standard-commands self the-class (class-prototype the-class)))))))

(def render standard-object-list-inspector ()
  (bind (((:read-only-slots id content) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (call-next-method)))
      (if (typep content 'reference-component)
          <span (:id ,id :class "standard-object-list-inspector")
            ,(body)>
          <div (:id ,id :class "standard-object-list-inspector")
            ,(body)>))))

(def (layered-function e) make-standard-object-list-inspector-alternatives (component class prototype instances)
  (:method ((component standard-object-list-inspector) (class standard-class) (prototype standard-object) (instances list))
    (list (delay-alternative-component-with-initargs 'standard-object-list-table-inspector :the-class class :instances instances)
          (delay-alternative-component-with-initargs 'standard-object-list-list-inspector :the-class class :instances instances)
          (delay-alternative-reference-component 'standard-object-list-inspector-reference instances))))

(def layered-method make-standard-commands ((component standard-object-list-inspector) (class standard-class) (prototype standard-object))
  (make-editing-commands component))

;;;;;;
;;; Standard object list list inspector

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
    (make-viewer instance :default-component-type 'reference-component)))

;;;;;;
;;; Standard object table inspector

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
                         (setf row (make-standard-object-list-table-row self the-class instance)))
                     (collect row)))))

(def (layered-function e) make-standard-object-list-table-row (component class instance)
  (:method ((component standard-object-list-table-inspector) (class standard-class) (instance standard-object))
    (make-instance 'standard-object-row-inspector :instance instance)))

(def (function e) make-standard-object-list-table-type-column ()
  (make-instance 'column-component
                 :content (label #"object-list-table.column.type")
                 :cell-factory (lambda (row-component)
                                 (make-instance 'cell-component
                                                :content (bind ((class (class-of (instance-of row-component))))
                                                           (make-class-presentation row-component class (class-prototype class)))))))

(def (function e) make-standard-object-list-table-command-bar-column ()
  (make-instance 'column-component
                 :content (label #"object-list-table.column.commands")
                 :visible (delay (not (layer-active-p 'passive-components-layer)))
                 :cell-factory (lambda (row-component)
                                 (make-instance 'cell-component :content (command-bar-of row-component)))))

(def (layered-function e) make-standard-object-list-table-inspector-columns (component)
  (:method ((self standard-object-list-table-inspector))
    (append (optional-list (make-standard-object-list-table-command-bar-column)
                           (when-bind the-class (the-class-of self)
                             (when (closer-mop:class-direct-subclasses the-class)
                               (make-standard-object-list-table-type-column))))
            (make-standard-object-list-table-inspector-slot-columns self))))

(def (layered-function e) make-standard-object-list-table-inspector-slot-columns (component)
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
                                                       (make-instance 'cell-component :content "N/A"))))))
                (nreverse slot-name->slot-map))))))

(def (layered-function e) collect-standard-object-list-table-inspector-slots (component class instance)
  (:method ((component standard-object-list-table-inspector) (class standard-class) (instance standard-object))
    (class-slots class)))

(def render standard-object-list-table-inspector ()
  <span ,(standard-object-list-table-inspector.instances (localized-class-name (the-class-of -self-)))>
  (call-next-method))

(def resources en
  (standard-object-list-table-inspector.instances (class)
    <span "Viewing instances of " ,(render class)>)
  (object-list-table.column.commands "Commands")
  (object-list-table.column.type "Type"))

(def resources hu
  (standard-object-list-table-inspector.instances (class)
    <span "Egy ",(render class) " lista megjelenítése">)
  (object-list-table.column.commands "Műveletek")
  (object-list-table.column.type "Típus"))

;;;;;;
;;; Standard object row inspector

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
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-commands self (class-of instance) instance))
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

(def layered-method make-standard-commands ((component standard-object-row-inspector) (class standard-class) (instance standard-object))
  (append (make-editing-commands component)
          (list (make-expand-row-command component instance))))

(def function make-expand-row-command (component instance)
  (make-replace-and-push-back-command component (delay (make-instance '(editable-component entire-row-component) :content (make-viewer instance :default-component-type 'detail-component)))
                                      (list :icon (icon expand)
                                            :visible (delay (not (has-edited-descendant-component-p component))))
                                      (list :icon (icon collapse))))

;;;;;;
;;; Standard object slot value cell inspector

(def component standard-object-slot-value-cell-component (abstract-standard-object-slot-value-component
                                                          cell-component
                                                          editable-component)
  ())

(def method refresh-component ((component standard-object-slot-value-cell-component))
  (with-slots (instance slot content) component
    (if slot
        (setf content (make-instance 'place-inspector :place (make-slot-value-place instance slot)))
        (setf content nil))))

;;;;;;
;;; Standard object list inspector

(def component selectable-standard-object-list-inspector (standard-object-list-inspector)
  ())

(def method selected-instance-of ((self selectable-standard-object-list-inspector))
  (awhen (content-of self)
    (selected-instance-of it)))

(def layered-method make-standard-object-list-inspector-alternatives ((component selectable-standard-object-list-inspector) (class standard-class) (prototype standard-object) (instances list))
  (list (delay-alternative-component-with-initargs 'selectable-standard-object-list-table-inspector :the-class class :instances instances)
        (delay-alternative-component-with-initargs 'standard-object-list-list-inspector :the-class class :instances instances)
        (delay-alternative-reference-component 'standard-object-list-inspector-reference instances)))

;;;;;;
;;; Selectable standard object list table inspector

(def component selectable-standard-object-list-table-inspector (standard-object-list-table-inspector
                                                                abstract-selectable-standard-object-component)
  ())

(def layered-method make-standard-object-list-table-row ((component selectable-standard-object-list-table-inspector) (class standard-class) (instance standard-object))
  (make-instance 'selectable-standard-object-row-inspector :instance instance))

;;;;;;
;;; Selectable standard object row inspector

(def component selectable-standard-object-row-inspector (standard-object-row-inspector)
  ())

(def layered-method table-row-style-class ((table selectable-standard-object-list-table-inspector) (row selectable-standard-object-row-inspector))
  (concatenate-string (call-next-method)
                      (when (selected-instance-p table (instance-of row))
                        " selected")))

(def layered-method make-standard-commands ((component selectable-standard-object-row-inspector) (class standard-class) (instance standard-object))
  (list* (make-select-instance-command (find-ancestor-component-with-type component 'abstract-selectable-standard-object-component) instance)
         (call-next-method)))
