;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Reference

(def component reference-component ()
  ((target)
   (expand-command :type component)))

(def render reference-component ()
  (render (expand-command-of self)))

(def (generic e) make-reference-label (reference)
  (:method ((reference reference-component))
    (princ-to-string (target-of reference))))

(def (function e) make-expand-reference-command-component (original-component replacement-component)
  (make-replace-command-component original-component replacement-component
                                  :icon (make-icon-component 'expand :label (make-reference-label original-component) :tooltip "Show in detail")))

;;;;;;
;;; Reference list

(def component reference-list-component ()
  ((targets)
   (references :type components)))

(def constructor reference-list-component ()
  (with-slots (targets references) self
    (setf references
          (mapcar (lambda (target)
                    (make-viewer-component target :default-type 'reference-component))
                  targets))))

(def render reference-list-component ()
  <div ,@(mapcar #'render (references-of self))>)

;;;;;;
;;; Standard slot reference

(def component standard-slot-definition-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-slot-definition-reference-component))
  (full-symbol-name (slot-definition-name (target-of reference))))

;;;;;;
;;; Standard class reference

(def component standard-class-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-class-reference-component))
  (full-symbol-name (class-name (target-of reference))))

;;;;;;
;;; Standard object reference

(def component standard-object-reference-component (reference-component)
  ())

(def method make-reference-label ((reference standard-object-reference-component))
  (princ-to-string (target-of reference)))
