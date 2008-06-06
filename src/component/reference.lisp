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
  (setf (target-of self) new-value)
  (setf (label-of (icon-of (expand-command-of self))) (make-reference-label self)))

(def render reference-component ()
  (render (expand-command-of -self-)))

(def (generic e) make-reference-label (reference)
  (:method ((reference reference-component))
    (princ-to-string (target-of reference))))

(def (function e) make-expand-reference-command (original-component replacement-component)
  (make-replace-command original-component replacement-component
                        :icon (icon expand :label (make-reference-label original-component))))

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

(def component standard-slot-definition-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-slot-definition-reference-component))
  (qualified-symbol-name (slot-definition-name (target-of reference))))

;;;;;;
;;; Standard class reference

(def component standard-class-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-class-reference-component))
  (qualified-symbol-name (class-name (target-of reference))))

;;;;;;
;;; Standard object reference

(def component standard-object-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-object-reference-component))
  (princ-to-string (target-of reference)))

(def method render :before ((self standard-object-reference-component))
  (bind ((instance (target-of self)))
    (if (and (typep instance 'prc::persistent-object)
             (prc::persistent-p instance))
        (prc::revive-instance (target-of self)))))
;;;;;;
;;; Standard object list reference

(def component standard-object-list-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-object-list-reference-component))
  (bind ((*print-length* 3))
    (princ-to-string (target-of reference))))

;;;;;;
;;; Standard object filter reference

(def component standard-object-filter-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-object-filter-reference-component))
  (qualified-symbol-name (class-name (target-of reference))))

;;;;;;
;;; Standard object maker reference

(def component standard-object-maker-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-object-maker-reference-component))
  (qualified-symbol-name (class-name (target-of reference))))
