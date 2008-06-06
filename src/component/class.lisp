;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard class

(def component abstract-standard-class-component (value-component)
  ((the-class nil :type (or null standard-class)))
  (:documentation "Base class with a STANDARD-CLASS component value"))

(def method component-value-of ((component abstract-standard-class-component))
  (the-class-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-class-component))
  (setf (the-class-of component) new-value))

;;;;;;
;;; Standard class

(def component standard-class-component (abstract-standard-class-component alternator-component)
  ()
  (:documentation "Component for an instance of STANDARD-CLASS in various alternative views"))

(def method (setf component-value-of) :after (new-value (component standard-class-component))
  (with-slots (the-class default-component-type alternatives content command-bar) component
    (if the-class
        (progn
          (if alternatives
              (dolist (alternative alternatives)
                (setf (component-value-of alternative) the-class))
              (setf alternatives (list (delay-alternative-component-type 'standard-class-detail-component :the-class the-class)
                                       (delay-alternative-component 'standard-class-reference-component
                                         (setf-expand-reference-to-default-alternative-command (make-instance 'standard-class-reference-component :target the-class))))))
          (if content
              (setf (component-value-of content) the-class)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-instance 'command-bar-component
                                           :commands (append (list (make-top-command component)
                                                                   (make-refresh-command component))
                                                             (make-alternative-commands component alternatives)))))
        (setf alternatives nil
              content nil))))

;;;;;;
;;; Standard class detail

(def component standard-class-detail-component (abstract-standard-class-component detail-component)
  ((metaclass nil :type component)
   (direct-subclasses nil :type component)
   (direct-superclasses nil :type component)
   (direct-slots nil :type component)
   (effective-slots nil :type component))
  (:documentation "Component for an instance of STANDARD-CLASS in detail"))

(def method (setf component-value-of) :after (new-value (component standard-class-detail-component))
     (with-slots (the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) component
       (if the-class
           (progn
             (if metaclass
                 (setf (component-value-of metaclass) (class-of the-class))
                 (setf metaclass (make-viewer-component (class-of the-class) :default-component-type 'reference-component)))
             (if direct-subclasses
                 (setf (component-value-of direct-subclasses) (class-direct-subclasses the-class))
                 (setf direct-subclasses (make-instance 'reference-list-component :targets (class-direct-subclasses the-class))) )
             (if direct-superclasses
                 (setf (component-value-of direct-superclasses) (class-direct-superclasses the-class))
                 (setf direct-superclasses (make-instance 'reference-list-component :targets (class-direct-superclasses the-class))))
             (if direct-slots
                 (setf (component-value-of direct-slots) (class-direct-slots the-class))
                 (setf direct-slots (make-instance 'standard-slot-definition-table-component :slots (class-direct-slots the-class))))
             (if effective-slots
                 (setf (component-value-of effective-slots) (progn (ensure-finalized the-class) (class-slots the-class)))
                 (setf effective-slots (make-instance 'standard-slot-definition-table-component :slots (progn (ensure-finalized the-class) (class-slots the-class))))))
           (setf metaclass nil
                 direct-subclasses nil
                 direct-superclasses nil
                 direct-slots nil
                 effective-slots nil))))

(def render standard-class-detail-component ()
  (with-slots (the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) -self-
    (bind ((class-name (full-symbol-name (class-name the-class))))
      <div
        <span "The class " <i ,class-name> " is an instance of " ,(render metaclass)>
        <div
          <h3 "Direct super classes">
          ,(render direct-superclasses)>
        <div
          <h3 "Direct sub classes">
          ,(render direct-subclasses)>
        <div
          <h3 "Direct slots">
          ,(render direct-slots)>
        <div
          <h3 "Effective slots">
          ,(render effective-slots)>>)))

;;;;;;
;;; Abstract standard slot definition detail

(def component abstract-standard-slot-definition-group-component (value-component)
  ((the-class nil :type (or null standard-class))
   (slots nil :type list))
  (:documentation "Base class with a list of STANDARD-SLOT-DEFINITIONs component value"))

(def method component-value-of ((component abstract-standard-slot-definition-group-component))
  (slots-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-slot-definition-group-component))
  (setf (slots-of component) new-value))

;;;;;;
;;; Standard slot definition table

(def component standard-slot-definition-table-component (abstract-standard-slot-definition-group-component table-component)
  ()
  (:documentation "Component for a list of STANDARD-SLOT-DEFINITIONs instances as a table"))

(def constructor standard-slot-definition-table-component ()
  (setf (columns-of -self-)
        (list
         (make-instance 'column-component :content (make-instance 'label-component :component-value "Name"))
         (make-instance 'column-component :content (make-instance 'label-component :component-value "Type"))
         (make-instance 'column-component :content (make-instance 'label-component :component-value "Readers"))
         (make-instance 'column-component :content (make-instance 'label-component :component-value "Writers")))))

(def method (setf component-value-of) :after (new-value (component standard-slot-definition-table-component))
  (with-slots (slots columns rows) component
    (setf rows
          (iter (for slot :in slots)
                (for row = (find slot rows :key #'component-value-of))
                (if row
                    (setf (component-value-of row) slot)
                    (setf row (make-instance 'standard-slot-definition-row-component :slot slot)))
                (collect row)))))

;;;;;;
;;; Abstract standard slot definition

(def component abstract-standard-slot-definition-component (value-component)
  ((the-class nil :type (or null standard-class))
   (slot nil :type (or null standard-slot-definition)))
  (:documentation "Base class with a STANDARD-SLOT-DEFINITION component value"))

(def method component-value-of ((component abstract-standard-slot-definition-component))
  (slot-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-slot-definition-component))
  (setf (slot-of component) new-value))

;;;;;;
;;; Standard slot definition row

(def component standard-slot-definition-row-component (abstract-standard-slot-definition-component row-component)
  ((label nil :type component)
   (type nil :accessor nil :type component)
   (readers nil :type component)
   (writers nil :type component))
  (:documentation "Component for a STANDARD-SLOT-DEFINITION as a table row"))

(def method (setf component-value-of) :after (new-value (component standard-slot-definition-row-component))
  (with-slots (slot label type readers writers cells) component
    (if slot
        (setf label (make-instance 'string-component :component-value (full-symbol-name (slot-definition-name slot)))
              type (make-instance 'string-component :component-value (string-downcase (princ-to-string (slot-type slot))))
              readers (make-instance 'string-component :component-value (string-downcase (princ-to-string (slot-definition-readers slot))))
              writers (make-instance 'string-component :component-value (string-downcase (princ-to-string (slot-definition-writers slot))))
              cells (mapcar (lambda (content)
                              (make-instance 'cell-component :content content))
                            (list label type readers writers)))
        (setf label nil
              type nil
              readers nil
              writers nil
              cells nil))))

;;;;;;
;;; Standard slot definition

;; TODO: fill in stuff
(def component standard-slot-definition-component (abstract-standard-slot-definition-component alternator-component)
  ()
  (:documentation "Component for an instance of STANDARD-SLOT-DEFINITION in various alternative views"))

;;;;;;
;;; Standard slot definition detail

;; TODO: fill in stuff
(def component standard-slot-definition-detail-component (abstract-standard-slot-definition-component detail-component)
  ()
  (:documentation "Component for an instance of STANDARD-SLOT-DEFINITION in detail"))
