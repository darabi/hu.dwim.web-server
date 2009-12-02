;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; sequence/inspector

(def (component e) sequence/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* sequence sequence/inspector)

(def layered-method make-alternatives ((component sequence/inspector) class prototype value)
  (list (delay-alternative-component-with-initargs 'sequence/table/inspector :component-value value)
        (delay-alternative-component-with-initargs 'sequence/list/inspector :component-value value)
        (delay-alternative-component-with-initargs 'sequence/tree/inspector :component-value value)
        (delay-alternative-reference 'sequence/reference/inspector value)))

;;;;;;
;;; sequence/reference/inspector

(def (component e) sequence/reference/inspector (t/reference/inspector)
  ())

;;;;;;
;;; sequence/list/inspector

(def (component e) sequence/list/inspector (inspector/style t/detail/presentation list/widget)
  ())

(def refresh-component sequence/list/inspector
  (bind (((:slots component-value contents) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf contents (iter (for element-value :in-sequence component-value)
                         (collect (make-list/element -self- class prototype element-value))))))

(def layered-method make-page-navigation-bar ((component sequence/list/inspector) class prototype value)
  (make-instance 'page-navigation-bar/widget :total-count (length value)))

(def (layered-function e) make-list/element (component class prototype value)
  (:method ((component sequence/list/inspector) class prototype value)
    (make-instance 't/element/inspector :component-value value)))

;;;;;;
;;; t/element/inspector

(def (component e) t/element/inspector (inspector/style element/widget)
  ())

(def refresh-component t/element/inspector
  (bind (((:slots component-value content) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-element/content -self- class prototype component-value)))))

(def layered-function make-element/content (component class prototype value)
  (:method ((component t/element/inspector) class prototype value)
    (make-value-inspector value :initial-alternative-type 't/reference/inspector)))

;;;;;;
;;; sequence/table/inspector

(def (component e) sequence/table/inspector (inspector/style
                                             t/detail/presentation
                                             table/widget
                                             component-messages/widget)
  ())

(def method component-dispatch-class ((component sequence/table/inspector))
  ;; TODO: KLUDGE: this should be an argument
  (class-of (first-elt (component-value-of component))))

(def refresh-component sequence/table/inspector
  (bind (((:slots component-value rows columns) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf columns (make-table-columns -self- class prototype component-value)
          rows (iter (for row-value :in-sequence component-value)
                     (for row = (find row-value rows :key #'component-value-of))
                     (if row
                         (setf (component-value-of row) row-value)
                         (setf row (make-table-row -self- class prototype row-value)))
                     (collect row)))))

(def layered-method make-page-navigation-bar ((component sequence/table/inspector) class prototype value)
  (make-instance 'page-navigation-bar/widget :total-count (length value)))

(def (layered-function e) make-table-row (component class prototype value)
  (:method ((component sequence/table/inspector) class prototype value)
    (make-instance 't/row/inspector :component-value value)))

(def (layered-function e) make-table-type-column (component class prototype value)
  (:method ((component sequence/table/inspector) class prototype value)
    (make-instance 'place/column/inspector
                   :component-value "BLAH" ;; TODO:
                   :header #"object-list-table.column.type"
                   :cell-factory (lambda (row)
                                   (bind ((class (class-of (component-value-of row))))
                                     (make-value-inspector class :initial-alternative-type 't/reference/inspector))))))

(def (layered-function e) make-table-columns (component class prototype value)
  (:method ((component sequence/table/inspector) class prototype value)
    (append (optional-list (when-bind the-class (component-dispatch-class component)
                             (when (closer-mop:class-direct-subclasses the-class)
                               (make-table-type-column component class prototype value))))
            (make-table-place-columns component class prototype value))))

(def (layered-function e) make-table-place-columns (component class prototype value)
  (:method ((component sequence/table/inspector) class prototype value)
    (bind (((:slots command-bar columns rows component-value) component)
           (slot-name->slot-map nil))
      ;; KLUDGE: TODO: this register mapping is wrong, maps slot-names to randomly choosen effective-slots, must be forbidden
      (flet ((register-slot (slot)
               (bind ((slot-name (slot-definition-name slot)))
                 (unless (member slot-name slot-name->slot-map :test #'eq :key #'car)
                   (push (cons slot-name slot) slot-name->slot-map)))))
        (when class
          (foreach #'register-slot (collect-slot-value-list/slots component class (class-prototype class) component-value)))
        (iter (for value :in-sequence component-value)
              (foreach #'register-slot (collect-slot-value-list/slots component (class-of value) value component-value))))
      (mapcar (lambda (slot-name->slot)
                (make-instance 'place/column/inspector
                               :component-value "BLAH" ;; TODO:
                               :header (localized-slot-name (cdr slot-name->slot))
                               :cell-factory (lambda (row-component)
                                               (bind ((slot (find-slot (class-of (component-value-of row-component)) (car slot-name->slot)
                                                                       :otherwise nil)))
                                                 (if slot
                                                     (make-instance 'place/cell/inspector
                                                                    :component-value (make-object-slot-place (component-value-of row-component) slot))
                                                     (empty/layout/singleton))))))
              (nreverse slot-name->slot-map)))))

(def layered-method collect-slot-value-list/slots ((component sequence/table/inspector) class prototype value)
  (class-slots class))

;;;;;;
;;; place/column/inspector

(def (component e) place/column/inspector (inspector/basic column/widget)
  ((cell-factory :type (or symbol function))))

;;;;;;
;;; t/row/inspector

(def (component e) t/row/inspector (inspector/style
                                    t/detail/presentation
                                    row/widget
                                    component-messages/widget)
  ())

(def refresh-component t/row/inspector
  (bind (((:slots component-value command-bar cells) -self-))
    (setf cells
          (if component-value
              (mapcar (lambda (column)
                        (funcall (cell-factory-of column) -self-))
                      (columns-of *table*))
              nil))))

(def layered-method render-table-row :before ((table sequence/table/inspector) (row t/row/inspector))
  (when (messages-of row)
    (render-table-row table (make-instance 'entire-row/widget :content (inline-render-component/widget ()
                                                                         (render-component-messages-for row))))))

(def layered-method render-onclick-handler ((row t/row/inspector) button)
  (when-bind expand-command (find-command row 'expand-component)
    (render-command-onclick-handler expand-command (id-of row))))

(def layered-method make-context-menu-items ((component t/row/inspector) class prototype value)
  (append (optional-list (make-expand-command component class prototype value)) (call-next-method)))

(def layered-method make-command-bar-commands ((component t/row/inspector) class prototype value)
  nil)

(def layered-method make-move-commands ((component t/row/inspector) class prototype value)
  nil)

(def layered-method make-expand-command ((component t/row/inspector) class prototype value)
  (bind ((replacement-component nil))
    (make-replace-and-push-back-command component
                                        (delay (setf replacement-component
                                                     (make-instance 't/entire-row/inspector :component-value value)))
                                        (list :content (icon expand-component) :visible (delay (not (has-edited-descendant-component-p component))) :ajax (delay (id-of component)))
                                        (list :content (icon collapse-component) :ajax (delay (id-of replacement-component))))))

;;;;;;
;;; t/entire-row/inspector

(def (component e) t/entire-row/inspector (inspector/style
                                           t/detail/presentation
                                           entire-row/widget
                                           component-messages/widget)
  ())

(def refresh-component t/entire-row/inspector
  (bind (((:slots component-value content) -self-))
    (setf content
          (if component-value
              (make-value-inspector component-value)
              (empty/layout)))))

;;;;;;
;;; place/cell/inspector

(def (component e) place/cell/inspector (inspector/style t/detail/presentation cell/widget)
  ())

(def refresh-component place/cell/inspector
  (bind (((:slots component-value content) -self-))
    (if component-value
        (setf content (make-instance 'place/value/inspector :component-value component-value))
        (setf content (empty/layout)))))
