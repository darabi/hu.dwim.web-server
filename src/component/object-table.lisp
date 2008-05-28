;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object table

(def component standard-object-table-component (table-component editable-component)
  ((instances)
   (slot-names)
   (columns nil :type components)
   (rows nil :type components)))

(def constructor standard-object-table-component ()
  (with-slots (instances slot-names columns rows) self
    (setf slot-names (delete-duplicates
                      (iter (for instance :in instances)
                            (appending (mapcar 'slot-definition-name (class-slots (class-of instance))))))
          columns (cons (make-instance 'label-component :component-value "Commands")
                        (mapcar (lambda (slot-name)
                                  (make-instance 'label-component :component-value (full-symbol-name slot-name)))
                                slot-names))
          rows (mapcar (lambda (instance)
                         (make-instance 'standard-object-row-component :table-slot-names slot-names :instance instance))
                       instances))))

;;;;;;
;;; Standard object row

(def component standard-object-row-component (row-component editable-component)
  ((instance)
   (table-slot-names)
   (command-bar nil :type component)
   (cells nil :type components)))

(def constructor standard-object-row-component ()
  (with-slots (instance table-slot-names command-bar cells) self
    (setf command-bar (make-instance 'command-bar-component :commands (list (make-expand-row-command-component self instance)
                                                                            (make-begin-editing-command-component self)
                                                                            (make-save-editing-command-component self)
                                                                            (make-cancel-editing-command-component self)))
          cells (cons (make-instance 'cell-component :content command-bar)
                      (iter (for class = (class-of instance))
                            (for table-slot-name :in table-slot-names)
                            (for slot = (find-slot class table-slot-name))
                            (collect (if slot
                                         (make-instance 'standard-object-slot-value-cell-component :instance instance :slot slot)
                                         (make-instance 'cell-component :content (make-instance 'string-component :component-value "N/A")))))))))

(def function make-expand-row-command-component (component instance)
  (make-replace-and-push-back-command-component component (delay (make-instance 'entire-row-component :content (make-viewer-component instance :default-component-type 'detail-component)))
                                                (list :icon (make-icon-component 'expand :label "Expand" :tooltip "Show in detail")
                                                      :visible (delay (not (has-edited-descendant-component-p component))))
                                                (list :icon (make-icon-component 'collapse :label "Collapse" :tooltip "Collapse to row"))))

;;;;;;
;;; Standard object slot value cell

(def component standard-object-slot-value-cell-component (cell-component editable-component)
  ((content :type component)))

(def constructor standard-object-slot-value-cell-component ()
  #+nil
  (setf content
        (make-instance 'place-component :place (if value
                                                   (make-slot-value-place component-value slot)
                                                   (make-phantom-slot-value-place the-class slot)))))
