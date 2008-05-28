;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object table

(def component standard-object-table-component (table-component editable-component)
  ((instances nil)
   (slot-names nil)))

(def constructor standard-object-table-component ()
  (awhen (instances-of self)
    (setf (component-value-of self) it)))

(def method component-value-of ((component standard-object-table-component))
  (instances-of component))

(def method (setf component-value-of) (new-value (component standard-object-table-component))
  (with-slots (instances slot-names columns rows) component
    (setf instances new-value)
    (if instances
        (setf slot-names (delete-duplicates
                          (iter (for instance :in instances)
                                (appending (mapcar 'slot-definition-name (class-slots (class-of instance))))))
              columns (cons (make-instance 'label-component :component-value "Commands")
                            (mapcar (lambda (slot-name)
                                      (make-instance 'label-component :component-value (full-symbol-name slot-name)))
                                    slot-names))
              rows (iter (for instance :in instances)
                         (for row = (find instance rows :key #'component-value-of))
                         (if row
                             (setf (component-value-of row) instance)
                             (setf row (make-instance 'standard-object-row-component :instance instance :table-slot-names slot-names)))
                         (collect row)))
        (setf slot-names nil
              columns nil
              rows nil))))

;;;;;;
;;; Standard object row

(def component standard-object-row-component (row-component editable-component)
  ((instance)
   (table-slot-names)
   (command-bar nil :type component)))

(def constructor standard-object-row-component ()
  (awhen (instance-of self)
    (setf (component-value-of self) it)))

(def method component-value-of ((component standard-object-row-component))
  (instance-of component))

(def method (setf component-value-of) (new-value (component standard-object-row-component))
  (with-slots (instance table-slot-names command-bar cells) component
    (setf instance new-value)
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (list (make-expand-row-command-component component instance)
                                                                                (make-begin-editing-command-component component)
                                                                                (make-save-editing-command-component component)
                                                                                (make-cancel-editing-command-component component)))
              cells (cons (make-instance 'cell-component :content command-bar)
                          (iter (for class = (class-of instance))
                                (for table-slot-name :in table-slot-names)
                                (for slot = (find-slot class table-slot-name))
                                (for cell = (find slot cells :key #'component-value-of))
                                (collect (if slot
                                             (make-instance 'standard-object-slot-value-cell-component :instance instance :slot slot)
                                             (make-instance 'cell-component :content (make-instance 'label-component :component-value "N/A")))))))
        (setf command-bar nil
              cells nil))))

(def function make-expand-row-command-component (component instance)
  (make-replace-and-push-back-command-component component (delay (make-instance 'entire-row-component :content (make-viewer-component instance :default-component-type 'detail-component)))
                                                (list :icon (make-icon-component 'expand :label "Expand" :tooltip "Show in detail")
                                                      :visible (delay (not (has-edited-descendant-component-p component))))
                                                (list :icon (make-icon-component 'collapse :label "Collapse" :tooltip "Collapse to row"))))

;;;;;;
;;; Standard object slot value cell

(def component standard-object-slot-value-cell-component (cell-component editable-component)
  ((instance)
   (slot)))

(def constructor standard-object-slot-value-cell-component ()
  (awhen (slot-of self)
    (setf (component-value-of self) it)))

(def method component-value-of ((component standard-object-slot-value-cell-component))
  (slot-of component))

(def method (setf component-value-of) (new-value (component standard-object-slot-value-cell-component))
  (with-slots (instance slot content) component
    (if slot
        (setf content (make-instance 'place-component :place (make-slot-value-place instance slot)))
        (setf content nil))))
