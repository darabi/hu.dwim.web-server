;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard slot definition mixin

(def (component e) standard-slot-definition/mixin ()
  ((slot
    nil
    :type (or null standard-slot-definition)
    :computed-in compute-as))
  (:documentation "A COMPONENT with a STANDARD-SLOT-DEFINITION."))

;;;;;;
;;; Standard slot definition abstract

(def (component e) standard-slot-definition/abstract (standard-slot-definition/mixin standard-class/mixin)
  ()
  (:documentation "A COMPONENT with a STANDARD-SLOT-DEFINITION component value and the corresponding STANDARD-CLASS."))

(def method component-value-of ((component standard-slot-definition/abstract))
  (slot-of component))

(def method (setf component-value-of) (new-value (self standard-slot-definition/abstract))
  (setf (slot-of self) new-value))

(def method clone-component ((self standard-slot-definition/abstract))
  (prog1-bind clone (call-next-method)
    (setf (the-class-of clone) (the-class-of self))))

;;;;;;
;;; Standard slot definition list mixin

(def (component e) standard-slot-definition-list/mixin ()
  ((the-class nil :type (or null standard-class))
   (slots nil :type list))
  (:documentation "A COMPONENT with a LIST of STANDARD-SLOT-DEFINITION instances as component value and the corresponding STANDARD-CLASS."))

(def method component-value-of ((component standard-slot-definition-list/mixin))
  (slots-of component))

(def method (setf component-value-of) (new-value (component standard-slot-definition-list/mixin))
  (setf (slots-of component) new-value))

(def method clone-component ((self standard-slot-definition-list/mixin))
  (prog1-bind clone (call-next-method)
    (setf (the-class-of clone) (the-class-of self))))

;;;;;;
;;; Standard slot definition table inspector

(def (component e) standard-slot-definition/table/inspector (standard-slot-definition-list/mixin table/widget)
  ((columns
    (list
     (column #"Column.name")
     (column #"Column.type")
     (column #"Column.readers")
     (column #"Column.writers"))))
  (:documentation "A COMPONENT for a list of STANDARD-SLOT-DEFINITIONs instances as a table"))

(def refresh-component standard-slot-definition/table/inspector
  (bind (((:slots slots columns rows) -self-))
    (setf rows
          (iter (for slot :in slots)
                (for row = (find slot rows :key #'component-value-of))
                (if row
                    (setf (component-value-of row) slot)
                    (setf row (make-instance 'standard-slot-definition/row/inspector :slot slot)))
                (collect row)))))

;;;;;;
;;; Standard slot definition row

(def (component e) standard-slot-definition/row/inspector (standard-slot-definition/abstract row/widget)
  ((label nil :type component)
   (the-type nil :type component)
   (readers nil :type component)
   (writers nil :type component))
  (:documentation "A COMPONENT for a STANDARD-SLOT-DEFINITION as a table row"))

(def refresh-component standard-slot-definition/row/inspector
  (bind (((:slots slot label the-type readers writers cells) -self-))
    (if slot
        (setf label (qualified-symbol-name (slot-definition-name slot))
              the-type (string-downcase (princ-to-string (slot-type slot)))
              readers (string-downcase (princ-to-string (slot-definition-readers slot)))
              writers (string-downcase (princ-to-string (slot-definition-writers slot)))
              cells (list label type readers writers))
        (setf label nil
              the-type nil
              readers nil
              writers nil
              cells nil))))

;;;;;;
;;; Standard slot definition

;; TODO: fill in stuff
(def (component e) standard-slot-definition/inspector (standard-slot-definition/abstract alternator/widget)
  ()
  (:documentation "A COMPONENT for an instance of STANDARD-SLOT-DEFINITION in various alternative views"))

(def layered-method make-alternatives ((component standard-slot-definition/inspector) (class standard-class) (prototype standard-slot-definition) (instance standard-slot-definition))
  (list (delay-alternative-component-with-initargs 'standard-slot-definition/detail/inspector :the-class class)
        (delay-alternative-reference-component 'standard-slot-definition/reference/inspector class)))

;;;;;;
;;; Standard slot definition detail

;; TODO: fill in stuff
(def (component e) standard-slot-definition/detail/inspector (standard-slot-definition/abstract detail/abstract)
  ()
  (:documentation "A COMPONENT for an instance of STANDARD-SLOT-DEFINITION in detail view."))

;;;;;;
;;; Standard slot definition reference

(def (component e) standard-slot-definition/reference (reference-component)
  ()
  (:documentation "A COMPONENT for an instance of STANDARD-SLOT-DEFINITION in detail view."))

(def method make-reference-label ((reference standard-slot-definition/reference) (class standard-class) (slot standard-slot-definition))
  (qualified-symbol-name (slot-definition-name slot)))
