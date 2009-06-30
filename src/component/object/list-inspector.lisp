;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list inspector

(def (component e) standard-object-list-inspector (standard-object-list/mixin
                                                  standard-class/mixin
                                                  inspector/abstract
                                                  editable/mixin
                                                  exportable/abstract
                                                  alternator/basic
                                                  initargs/mixin
                                                  layer-context-capturing/mixin)
  ()
  (:default-initargs :the-class (find-class 'standard-object))
  (:documentation "Inspector for a list of STANDARD-OBJECT instances in various alternative views."))

(def (macro e) standard-object-list-inspector (instances &rest args)
  `(make-instance 'standard-object-list-inspector :instances ,instances ,@args))

(def layered-method make-title ((self standard-object-list-inspector))
  (title (standard-object-list-inspector.title (localized-class-name (the-class-of self)))))

(def layered-method make-alternatives ((component standard-object-list-inspector) (class standard-class) (prototype standard-object) (instances list))
  (list (delay-alternative-component-with-initargs 'standard-object-list-table-inspector :the-class class :instances instances)
        (delay-alternative-component-with-initargs 'standard-object-list-list-inspector :the-class class :instances instances)
        (delay-alternative-reference-component 'standard-object-list-inspector-reference instances)))

(def layered-method make-context-menu-items ((component standard-object-list-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (optional-list (make-begin-editing-new-instance-command component class instance)) (call-next-method)))

(def (layered-function e) make-begin-editing-new-instance-command (component class instance)
  (:method ((component standard-object-list-inspector) (class standard-class) (instance standard-object))
    (command ()
      (icon new)
      (make-action
        (begin-editing-new-instance component class instance)))))

(def (layered-function e) begin-editing-new-instance (component class instance)
  (:method ((component standard-object-list-inspector) (class standard-class) (instance standard-object))
    (appendf (rows-of (content-of component))
             (list (make-instance 'entire-row-component :content (make-maker class))))))

(def layered-method create-instance ((ancestor standard-object-list-inspector) (component standard-object-maker) (class standard-class))
  (prog1-bind instance (call-next-method)
    (setf (component-at-place (make-component-place (parent-component-of component)))
          (make-standard-object-list-table-row (content-of ancestor) (class-of instance) instance))))

;;;;;;
;;; Standard object list list inspector

(def (component e) standard-object-list-list-inspector (standard-object-list/mixin
                                                       standard-class/mixin
                                                       inspector/abstract
                                                       list-component
                                                       editable/mixin)
  ((component-factory
    #'make-standard-object-list-component
    :type (or symbol function))))

(def refresh-component standard-object-list-list-inspector
  (bind (((:slots instances the-class components) -self-))
    (setf components
          (iter (for instance :in instances)
                (for component = (find instance components :key #'component-value-of))
                (if component
                    (setf (component-value-of component) instance)
                    (setf component (funcall (component-factory-of -self-) -self- (class-of instance) instance)))
                (collect component)))))

(def (generic e) make-standard-object-list-component (component class instance)
  (:method ((component standard-object-list-list-inspector) (class standard-class) (instance standard-object))
    (make-viewer instance :initial-alternative-type 'reference-component)))

;;;;;;
;;; Standard object table inspector

(def (component e) standard-object-list-table-inspector (standard-object-list/mixin
                                                        standard-class/mixin
                                                        inspector/abstract
                                                        table-component
                                                        editable/mixin)
  ()
  (:default-initargs :the-class (find-class 'standard-object)))

(def refresh-component standard-object-list-table-inspector
  (bind (((:slots instances the-class columns rows page-navigation-bar) -self-))
    (awhen (inherited-initarg -self- :page-size)
      (setf (page-size-of page-navigation-bar) it))
    (setf columns (make-standard-object-list-table-inspector-columns -self-))
    (setf rows (iter (for instance :in instances)
                     (for row = (find instance rows :key #'component-value-of))
                     (if row
                         (setf (component-value-of row) instance)
                         (setf row (make-standard-object-list-table-row -self- the-class instance)))
                     (collect row)))))

(def (layered-function e) make-standard-object-list-table-row (component class instance)
  (:method ((component standard-object-list-table-inspector) (class standard-class) (instance standard-object))
    (make-instance 'standard-object-row-inspector :instance instance)))

(def (function e) make-standard-object-list-table-type-column ()
  (make-instance 'column-component
                 :content #"object-list-table.column.type"
                 :cell-factory (lambda (row-component)
                                 (bind ((class (class-of (instance-of row-component))))
                                   (make-standard-class-presentation row-component class (class-prototype class))))))

(def (function e) make-standard-object-list-table-command-bar-column ()
  (make-instance 'column-component
                 :content #"object-list-table.column.commands"
                 :cell-factory #'context-menu-of))

(def (layered-function e) make-standard-object-list-table-inspector-columns (component)
  (:method ((self standard-object-list-table-inspector))
    (append (optional-list (make-standard-object-list-table-command-bar-column)
                           (when-bind the-class (the-class-of self)
                             (when (closer-mop:class-direct-subclasses the-class)
                               (make-standard-object-list-table-type-column))))
            (make-standard-object-list-table-inspector-slot-columns self))))

(def (layered-function e) make-standard-object-list-table-inspector-slot-columns (component)
  (:method ((self standard-object-list-table-inspector))
    (bind (((:slots instances the-class command-bar columns rows) self)
           (slot-name->slot-map nil))
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
                               :content (localized-slot-name (cdr slot-name->slot))
                               :cell-factory (lambda (row-component)
                                               (bind ((slot (find-slot (class-of (instance-of row-component)) (car slot-name->slot))))
                                                 (if slot
                                                     (make-instance 'standard-object-slot-value-cell-component :instance (instance-of row-component) :slot slot)
                                                     "")))))
              (nreverse slot-name->slot-map)))))

(def (layered-function e) collect-standard-object-list-table-inspector-slots (component class instance)
  (:method ((component standard-object-list-table-inspector) (class standard-class) (instance standard-object))
    (class-slots class)))

(def render-xhtml standard-object-list-table-inspector
  <div (:id ,(id-of -self-)) ,(call-next-method)>)

;;;;;;
;;; Standard object row inspector

(def (component e) standard-object-row-inspector (standard-object/mixin
                                                 inspector/abstract
                                                 editable/mixin
                                                 row-component
                                                 component-messages/basic
                                                 commands/mixin)
  ())

(def layered-method render-onclick-handler ((self standard-object-row-inspector) (button (eql :left)))
  (when-bind expand-command (find-command self 'expand)
    (render-command-onclick-handler expand-command (id-of self))))

(def refresh-component standard-object-row-inspector
  (bind (((:slots instance command-bar cells) -self-))
    (setf cells
          (if instance
              (mapcar (lambda (column)
                        (funcall (cell-factory-of column) -self-))
                      (columns-of *table*))
              nil))))

(def layered-method render-table-row ((table standard-object-list-table-inspector) (row standard-object-row-inspector))
  (when (messages-of row)
    (render-entire-row table row
                       (lambda ()
                         (render-component-messages row))))
  (call-next-method))

(def layered-method make-context-menu-items ((component standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (optional-list (make-expand-command component class prototype instance)) (call-next-method)))

(def layered-method make-command-bar-commands ((component standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  nil)

(def layered-method make-move-commands ((component standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  nil)

(def layered-method make-expand-command ((component standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  (bind ((replacement-component nil))
    (make-replace-and-push-back-command component
                                        (delay (setf replacement-component
                                                     (make-instance '(editable/mixin entire-row-component)
                                                                    :content (make-viewer instance :initial-alternative-type 'detail-component))))
                                        (list :content (icon expand) :visible (delay (not (has-edited-descendant-component-p component))) :ajax (delay (id-of component)))
                                        (list :content (icon collapse) :ajax (delay (id-of replacement-component))))))

;;;;;;
;;; Standard object slot value cell inspector

(def (component e) standard-object-slot-value-cell-component (standard-object-slot/mixin
                                                             cell-component
                                                             editable/mixin)
  ())

(def refresh-component standard-object-slot-value-cell-component
  (bind (((:slots instance slot content) -self-))
    (if slot
        (setf content (make-instance 'place-inspector :place (make-slot-value-place instance slot)))
        (setf content nil))))

;;;;;;
;;; Standard object list inspector

(def (component e) selectable-standard-object-list-inspector (standard-object-list-inspector)
  ())

(def method selected-instance-of ((self selectable-standard-object-list-inspector))
  (awhen (content-of self)
    (selected-instance-of it)))

(def layered-method make-alternatives ((component selectable-standard-object-list-inspector) (class standard-class) (prototype standard-object) (instances list))
  (list (delay-alternative-component-with-initargs 'selectable-standard-object-list-table-inspector :the-class class :instances instances)
        (delay-alternative-component-with-initargs 'standard-object-list-list-inspector :the-class class :instances instances)
        (delay-alternative-reference-component 'standard-object-list-inspector-reference instances)))

;;;;;;
;;; Selectable standard object list table inspector

(def (component e) selectable-standard-object-list-table-inspector (standard-object-list-table-inspector
                                                                   abstract-selectable-standard-object-component)
  ())

(def layered-method make-standard-object-list-table-row ((component selectable-standard-object-list-table-inspector) (class standard-class) (instance standard-object))
  (make-instance 'selectable-standard-object-row-inspector :instance instance))

;;;;;;
;;; Selectable standard object row inspector

(def (component e) selectable-standard-object-row-inspector (standard-object-row-inspector)
  ())

(def layered-method table-row-style-class ((table selectable-standard-object-list-table-inspector) (row selectable-standard-object-row-inspector))
  (concatenate-string (call-next-method)
                      (when (selected-instance-p table (instance-of row))
                        " selected")))

(def layered-method make-context-menu-items ((component selectable-standard-object-row-inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (call-next-method) (optional-list (make-select-instance-command component class prototype instance))))

(def layered-method render-onclick-handler ((self selectable-standard-object-row-inspector) (button (eql :left)))
  (if-bind select-command (find-command self 'select)
    (render-command-onclick-handler select-command (id-of self))
    (call-next-method)))

(def (icon e) move-backward)

(def (icon e) move-forward)

(def function make-move-backward-command (component)
  (bind ((place (place-of (find-ancestor-component-with-type component 'place-inspector)))
         (instances (value-at-place place))
         (instance (instance-of component))
         (position (position instance instances)))
    (unless (= position 0)
      (command ()
        (icon move-backward)
        (make-action
          (unless (= position 0)
            (setf (elt instances position) (elt instances (1- position))
                  (elt instances (1- position)) instance)))))))

(def function make-move-forward-command (component)
  (bind ((place (place-of (find-ancestor-component-with-type component 'place-inspector)))
         (instances (value-at-place place))
         (instance (instance-of component))
         (position (position instance instances)))
    (unless (= position (1- (length instances)))
      (command ()
        (icon move-forward)
        (make-action
          (unless (= position (1- (length instances)))
            (setf (elt instances position) (elt instances (1+ position))
                  (elt instances (1+ position)) instance)))))))
