;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object list

(def component abstract-standard-object-list-component (value-component)
  ((instances nil)))

(def method component-value-of ((component abstract-standard-object-list-component))
  (instances-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-list-component))
  (setf (instances-of component) new-value))

;;;;;;
;;; Standard object list

(def component standard-object-list-component (abstract-standard-object-list-component alternator-component editable-component)
  ())

(def method (setf component-value-of) :after (new-value (component standard-object-list-component))
  (with-slots (instances default-component-type alternatives content command-bar) component
    (setf instances new-value)
    (if instances
        (progn
          (if (and alternatives
                   (not (typep content 'empty-component)))
              (dolist (alternative alternatives)
                (setf (component-value-of (force alternative)) instances))
              (setf alternatives (list (delay-alternative-component-type 'standard-object-table-component :instances instances)
                                       (delay-alternative-component 'standard-object-list-reference-component
                                         (setf-expand-reference-to-default-alternative-command-component (make-instance 'standard-object-list-reference-component :target instances))))))
          (if (and content
                   (not (typep content 'empty-component)))
              (setf (component-value-of content) instances)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives)))))
        (setf alternatives (list (delay-alternative-component-type 'empty-component))
              content (find-default-alternative-component alternatives)))
    (setf command-bar (make-instance 'command-bar-component
                                     :commands (append (list (make-top-command-component component)
                                                             (make-refresh-command-component component))
                                                       (make-editing-command-components component)
                                                       (make-alternative-command-components component alternatives))))))

;;;;;;
;;; Standard object table

(def component standard-object-table-component (abstract-standard-object-list-component table-component editable-component)
  ((slot-names nil)))

(def method (setf component-value-of) :after (new-value (component standard-object-table-component))
  (with-slots (instances slot-names command-bar columns rows) component
    (setf instances new-value)
    (if instances
        (setf slot-names (delete-duplicates
                          (iter (for instance :in instances)
                                (appending (mapcar 'slot-definition-name (standard-object-table-slots (class-of instance) instance)))))
              columns (cons (make-instance 'column-component :content (make-instance 'label-component :component-value "Commands"))
                            (mapcar (lambda (slot-name)
                                      (make-instance 'column-component :content (make-instance 'label-component :component-value (full-symbol-name slot-name))))
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

(def generic standard-object-table-slots (class instance)
  (:method ((class standard-class) (instance standard-object))
    (class-slots class))

  (:method ((class prc::persistent-class) (instance prc::persistent-object))
    (prc::persistent-effective-slots-of class)))

;;;;;;
;;; Standard object row

(def component standard-object-row-component (abstract-standard-object-component row-component editable-component)
  ((table-slot-names)
   (command-bar nil :type component)))

(def method (setf component-value-of) :after (new-value (component standard-object-row-component))
  (with-slots (instance table-slot-names command-bar cells) component
    (setf instance new-value)
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (append (list (make-expand-row-command-component component instance))
                                                                                  (make-editing-command-components component)))
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
  (make-replace-and-push-back-command-component component (delay (make-instance '(editable-component entire-row-component) :content (make-viewer-component instance :default-component-type 'detail-component)))
                                                (list :icon (clone-icon 'expand)
                                                      :visible (delay (not (has-edited-descendant-component-p component))))
                                                (list :icon (clone-icon 'collapse))))

;;;;;;
;;; Standard object slot value cell

(def component standard-object-slot-value-cell-component (abstract-standard-object-slot-value-component cell-component editable-component)
  ())

(def method (setf component-value-of) :after (new-value (component standard-object-slot-value-cell-component))
  (with-slots (instance slot content) component
    (if slot
        (setf content (make-instance 'place-component :place (make-slot-value-place instance slot)))
        (setf content nil))))
