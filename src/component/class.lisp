;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard class

(def component standard-class-component (content-component #+nil alternator-component)
  ((the-class)))

(def constructor standard-class-component ()
  (with-slots (the-class content) self
    (setf content (make-instance 'standard-class-detail-component :the-class the-class)
          #+nil alternatives
          #+nil (list (delay-alternative-component-type 'standard-class-detail-component :class the-class)
                      (delay-alternative-component-type 'standard-class-reference-component :target the-class)))))

(def component standard-class-detail-component (detail-component)
  ((the-class)
   (metaclass :type component)
   (direct-subclasses :type component)
   (direct-superclasses :type component)
   (direct-slots :type component)
   (effective-slots :type component)))

(def constructor standard-class-detail-component ()
  (with-slots (the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) self
    (setf metaclass (make-viewer-component (class-of the-class) :default-component-type 'reference-component)
          direct-subclasses (make-instance 'reference-list-component :targets (class-direct-subclasses the-class))
          direct-superclasses (make-instance 'reference-list-component :targets (class-direct-superclasses the-class))
          direct-slots (make-instance 'slot-list-table-component :slots (class-direct-slots the-class))
          effective-slots (make-instance 'slot-list-table-component :slots (progn (ensure-finalized the-class) (class-slots the-class))))))

(def render standard-class-detail-component ()
  (with-slots (the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) self
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

(def component standard-slot-definition-table-component (table-component)
  ((slots)))

(def constructor standard-slot-definition-table-component ()
  (with-slots (slots columns rows) self
    (setf columns (list
                   (make-instance 'column-component :content (make-instance 'string-component :value "Name"))
                   (make-instance 'column-component :content (make-instance 'string-component :value "Type"))
                   (make-instance 'column-component :content (make-instance 'string-component :value "Readers"))
                   (make-instance 'column-component :content (make-instance 'string-component :value "Writers")))
          rows (mapcar (lambda (slot)
                         (make-instance 'slot-list-row-component :slot slot))
                       slots))))

(def component standard-slot-definition-row-component (row-component)
  ((slot)
   (label :type component)
   (type :accessor nil :type component)
   (readers :type component)
   (writers :type component)))

(def constructor standard-slot-definition-row-component ()
  (with-slots (slot label type readers writers cells) self
    (setf label (make-instance 'string-component :value (full-symbol-name (slot-definition-name slot)))
          type (make-instance 'string-component :value (string-downcase (princ-to-string (slot-definition-type slot))))
          readers (make-instance 'string-component :value (string-downcase (princ-to-string (slot-definition-readers slot))))
          writers (make-instance 'string-component :value (string-downcase (princ-to-string (slot-definition-writers slot))))
          cells (mapcar (lambda (content)
                          (make-instance 'cell-component :content content))
                        (list label type readers writers)))))

;;;;;;
;;; Standard slot definition

;; TODO:
(def component standard-slot-definition-component ()
  ())

;; TODO:
(def component standard-slot-definition-detail-component (detail-component)
  ())
