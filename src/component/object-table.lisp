;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard object list

(def component abstract-standard-object-list-component (value-component)
  ((instances nil)))

(def method component-value-of ((component abstract-standard-object-list-component))
  (instances-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-list-component))
  (setf (instances-of component) new-value))

(def render :before abstract-standard-object-list-component
  (with-slots (instances) -self-
    (setf instances (mapcar #'reuse-standard-object-instance instances))))

;;;;;;
;;; Standard object list

(def component standard-object-list-component (abstract-standard-object-list-component abstract-standard-class-component
                                               alternator-component editable-component user-message-collector-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-list-alternatives))

(def method refresh-component ((self standard-object-list-component))
  (with-slots (instances the-class default-component-type alternatives content command-bar) self
    (if alternatives
        (setf (component-value-for-alternatives self) instances)
        (setf alternatives (funcall (alternatives-factory-of self) the-class instances)))
    (if content
        (setf (component-value-of content) instances)
        (setf content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives))))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (append (list (make-open-in-new-frame-command self)
                                                                 (make-top-command self)
                                                                 (make-refresh-command self))
                                                           (make-standard-object-list-commands self the-class))))))

(def render standard-object-list-component ()
  <div (:class "standard-object-list")
    ,(render-user-messages -self-)
    ,(call-next-method)>)

(def (generic e) make-standard-object-list-alternatives (class instances)
  (:method (class instances)
    (list (delay-alternative-component-type 'standard-object-table-component :the-class class :instances instances)
          (delay-alternative-component-type 'list-component :elements instances)
          (delay-alternative-component 'standard-object-list-reference-component
            (setf-expand-reference-to-default-alternative-command
             (make-instance 'standard-object-list-reference-component :target instances))))))

(def generic make-standard-object-list-commands (component class)
  (:method ((component standard-object-list-component) class)
    nil)

  (:method ((component standard-object-list-component) (class standard-class))
    (make-editing-commands component))

  (:method ((component standard-object-list-component) (class prc::persistent-class))
    (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
      (make-editing-commands component))))

;;;;;;
;;; Standard object table

(def component standard-object-table-component (abstract-standard-object-list-component abstract-standard-class-component table-component editable-component)
  ())

(def method refresh-component ((self standard-object-table-component))
  (with-slots (instances the-class command-bar columns rows) self
    (setf columns (make-standard-object-table-columns self))
    (setf rows (iter (for instance :in instances)
                     (for row = (find instance rows :key #'component-value-of))
                     (if row
                         (setf (component-value-of row) instance)
                         (setf row (make-instance 'standard-object-row-component :instance instance)))
                     (collect row)))))

(def (function e) make-standard-object-table-type-column ()
  (make-instance 'column-component
                 :content (label #"Object-table.column.type")
                 :cell-factory (lambda (row-component)
                                 (make-instance 'cell-component :content (make-instance 'standard-class-component
                                                                                        :the-class (class-of (instance-of row-component))
                                                                                        :default-component-type 'reference-component)))))

(def (function e) make-standard-object-table-command-bar-column ()
  (make-instance 'column-component
                    :content (label #"Object-table.column.commands")
                    :visible (delay (not (layer-active-p 'passive-components-layer)))
                    :cell-factory (lambda (row-component)
                                    (make-instance 'cell-component :content (command-bar-of row-component)))))

(def (generic e) make-standard-object-table-columns (component)
  (:method ((self standard-object-table-component))
    (append (optional-list (make-standard-object-table-command-bar-column)
                           (when-bind the-class (the-class-of self)
                             (when (closer-mop:class-direct-subclasses the-class)
                               (make-standard-object-table-type-column))))
            (make-standard-object-table-slot-columns self))))

(def (generic e) make-standard-object-table-slot-columns (component)
  (:method ((self standard-object-table-component))
    (with-slots (instances the-class command-bar columns rows) self
      (bind ((slot-name->slot (list)))
        ;; KLUDGE: TODO: this register mapping is wrong, maps slot-names to randomly choosen effective-slots, must be forbidden
        (flet ((register-slot (slot)
                 (bind ((slot-name (slot-definition-name slot)))
                   (unless (member slot-name slot-name->slot :test #'eq :key #'car)
                     (push (cons slot-name slot) slot-name->slot)))))
          (when the-class
            (mapc #'register-slot (collect-standard-object-table-slots the-class (class-prototype the-class))))
          (dolist (instance instances)
            (mapc #'register-slot (collect-standard-object-table-slots (class-of instance) instance))))
        (mapcar (lambda (slot-name->slot)
                  (make-instance 'column-component
                                 :content (label (localized-slot-name (cdr slot-name->slot)))
                                 :cell-factory (lambda (row-component)
                                                 (bind ((slot (find-slot (class-of (instance-of row-component)) (car slot-name->slot))))
                                                   (if slot
                                                       (make-instance 'standard-object-slot-value-cell-component :instance (instance-of row-component) :slot slot)
                                                       (make-instance 'cell-component :content (label "N/A")))))))
                slot-name->slot)))))

(def (generic e) collect-standard-object-table-slots (class instance)
  (:method ((class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((class prc::persistent-class) (instance prc::persistent-object))
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((class dmm::entity) (instance prc::persistent-object))
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(defresources hu
  (object-table.column.commands "műveletek")
  (object-table.column.type "típus"))

(defresources en
  (object-table.column.commands "commands")
  (object-table.column.type "type"))

;;;;;;
;;; Standard object row

(def component standard-object-row-component (abstract-standard-object-component row-component editable-component user-message-collector-component-mixin)
  ((command-bar nil :type component)))

(def method refresh-component ((self standard-object-row-component))
  (with-slots (instance command-bar cells) self
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-object-row-commands self (class-of instance) instance))
              cells (mapcar (lambda (column)
                              (funcall (cell-factory-of column) self))
                            (columns-of (find-ancestor-component-with-type self 'standard-object-table-component))))
        (setf command-bar nil
              cells nil))))

(def render standard-object-row-component ()
  (when (messages-of -self-)
    (render-entire-row (parent-component-of -self-)
                       (lambda ()
                         (render-user-messages -self-))))
  (call-next-method))

(def (generic e) make-standard-object-row-commands (component class instance)
  (:method ((component standard-object-row-component) (class standard-class) (instance standard-object))
    (append (make-editing-commands component)
            (list (make-expand-row-command component instance))))

  (:method ((component standard-object-row-component) (class prc::persistent-class) (instance prc::persistent-object))
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

(def component standard-object-slot-value-cell-component (abstract-standard-object-slot-value-component cell-component editable-component)
  ())

(def method refresh-component ((component standard-object-slot-value-cell-component))
  (with-slots (instance slot content) component
    (if slot
        (setf content (make-instance 'place-component :place (make-slot-value-place instance slot)))
        (setf content nil))))
