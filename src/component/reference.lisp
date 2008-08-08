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
    (setf (label-of (icon-of expand-command)) (make-reference-label self (class-of target) target))))

(def render reference-component ()
  (render (expand-command-of -self-)))

(def render :in passive-components-layer reference-component
  ;; TODO this is not too nice this way
  <span ,(force (label-of (icon-of (expand-command-of -self-))))>)

(def (generic e) make-reference-label (component class target)
  (:method ((component reference-component) class target)
    (princ-to-string (target-of component))))

(def (function e) make-expand-reference-command (original-component replacement-component)
  (make-replace-command original-component replacement-component :icon (icon expand)))

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

(def method make-reference-label ((reference standard-slot-definition-reference) (class standard-class) (slot standard-slot-definition))
  (qualified-symbol-name (slot-definition-name slot)))

;;;;;;
;;; Standard class reference

(def component standard-class-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-class-reference) (metaclass standard-class) (class standard-class))
  (localized-class-name class :capitalize-first-letter #t))

;;;;;;
;;; Standard object reference

(def component standard-object-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-reference) (class standard-class) (instance standard-object))
  (princ-to-string instance))

(def method make-reference-label ((reference standard-object-reference) (class dmm::entity) (instance prc::persistent-object))
  (reuse-standard-object-reference reference)
  (localized-instance-name instance))

(def function reuse-standard-object-reference (self)
  (bind ((instance (target-of self)))
    (setf (target-of self) (reuse-standard-object-instance (class-of instance) instance))))

(def method refresh-component :before ((self standard-object-reference))
  (reuse-standard-object-reference self))

(def render :before standard-object-reference
  (reuse-standard-object-reference -self-))

;;;;;;
;;; Standard object list reference

(def component standard-object-list-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-list-reference) class (list list))
  (apply #'concatenate-string
         (append (list "(")
                 (iter (for index :from 0 :below 3)
                       (for element :in list)
                       (collect (unless (first-iteration-p) "; "))
                       (collect (make-reference-label reference (class-of element) element))
                       (when (= index 2)
                         (collect " ...")))
                 (list ")"))))

(def method make-reference-label ((reference standard-object-list-reference) (class dmm::entity) (instance prc::persistent-object))
  (localized-instance-name instance))

(def function reuse-standard-object-list-reference (self)
  ;; TODO: performance killer
  (setf (target-of self)
        (mapcar (lambda (instance)
                  (reuse-standard-object-instance (class-of instance) instance))
                (target-of self))))

(def method refresh-component :before ((self standard-object-list-reference))
  (reuse-standard-object-list-reference self))

(def render :before standard-object-list-reference
  (reuse-standard-object-list-reference -self-))

;;;;;;
;;; Standard object tree reference

(def component standard-object-tree-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-tree-reference) (class standard-class) (instance standard-object))
  (concatenate-string "Tree rooted at: " (princ-to-string instance)))

;;;;;;
;;; Standard object filter reference

(def component standard-object-filter-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-filter-reference) (metaclass standard-class) (class standard-class))
  (localized-class-name class :capitalize-first-letter #t))

;;;;;;
;;; Standard object maker reference

(def component standard-object-maker-reference (reference-component)
  ())

(def method make-reference-label ((reference standard-object-maker-reference) (metaclass standard-class) (class standard-class))
  (localized-class-name class :capitalize-first-letter #t))
