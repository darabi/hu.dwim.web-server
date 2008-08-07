;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Reference

(def component reference-component ()
  ((target)
   (expand-command :type component)))

(def method component-value-of ((self reference-component))
  (target-of self))

(def method (setf component-value-of) (new-value (self reference-component))
  (setf (target-of self) new-value))

(def method refresh-component ((self reference-component))
  (with-slots (target expand-command) self
    (setf (label-of (icon-of expand-command)) (make-reference-label (class-of target) target self))))

(def render reference-component ()
  (render (expand-command-of -self-)))

(def render :in passive-components-layer reference-component
  ;; TODO this is not too nice this way
  <span ,(force (label-of (icon-of (expand-command-of -self-))))>)

(def (generic e) make-reference-label (class target component)
  (:method (class target (component reference-component))
    (princ-to-string (target-of component))))

(def (function e) make-expand-reference-command (original-component replacement-component)
  (make-replace-command original-component replacement-component
                        :icon (icon expand :label (bind ((target (target-of original-component)))
                                                    (make-reference-label (class-of target) target original-component)))))

;;;;;;
;;; Reference list

(def component reference-list-component ()
  ((targets)
   (references :type components)))

(def constructor reference-list-component ()
  (with-slots (targets references) -self-
    (setf references
          (mapcar (lambda (target)
                    (make-viewer-component target :default-component-type 'reference-component))
                  targets))))

(def render reference-list-component ()
  <div ,@(mapcar #'render (references-of -self-))>)

;;;;;;
;;; Standard slot reference

(def component standard-slot-definition-reference (reference-component)
  ())

(def method make-reference-label ((class standard-class) (slot standard-slot-definition) (reference standard-slot-definition-reference))
  (qualified-symbol-name (slot-definition-name slot)))

;;;;;;
;;; Standard class reference

(def component standard-class-reference (reference-component)
  ())

(def method make-reference-label ((metaclass standard-class) (class standard-class) (reference standard-class-reference))
  (localized-class-name class :capitalize-first-letter #t))

;;;;;;
;;; Standard object reference

(def component standard-object-reference (reference-component)
  ())

(def method make-reference-label ((class standard-class) (instance standard-object) (reference standard-object-reference))
  (princ-to-string instance))

(def method make-reference-label ((class dmm::entity) (instance prc::persistent-object) (reference standard-object-reference))
  (localized-instance-name instance))

(def render :before standard-object-reference
  (bind ((instance (target-of -self-)))
    (if (and (typep instance 'prc::persistent-object)
             (prc::persistent-p instance))
        (prc::revive-instance (target-of -self-)))))
;;;;;;
;;; Standard object list reference

(def component standard-object-list-reference (reference-component)
  ())

(def method make-reference-label (class (list list) (reference standard-object-list-reference))
  (apply #'concatenate-string
         (append (list "(")
                 (iter (for index :from 0 :below 3)
                       (for element :in list)
                       (collect (unless (first-iteration-p) "; "))
                       (collect (make-reference-label (class-of element) element reference))
                       (when (= index 2)
                         (collect " ...")))
                 (list ")"))))

(def method make-reference-label ((class dmm::entity) (instance prc::persistent-object) (reference standard-object-list-reference))
  (localized-instance-name instance))

;;;;;;
;;; Standard object tree reference

(def component standard-object-tree-reference (reference-component)
  ())

(def method make-reference-label ((class standard-class) (instance standard-object) (reference standard-object-tree-reference))
  (concatenate-string "Tree rooted at: " (princ-to-string instance)))

;;;;;;
;;; Standard object filter reference

(def component standard-object-filter-reference (reference-component)
  ())

(def method make-reference-label ((metaclass standard-class) (class standard-class) (reference standard-object-filter-reference))
  (localized-class-name class :capitalize-first-letter #t))

;;;;;;
;;; Standard object maker reference

(def component standard-object-maker-reference (reference-component)
  ())

(def method make-reference-label ((metaclass standard-class) (class standard-class) (reference standard-object-maker-reference))
  (localized-class-name class :capitalize-first-letter #t))
