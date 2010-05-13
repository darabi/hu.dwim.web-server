;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; source-file/alternator/inspector

(def (component e) source-file/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null asdf:source-file) source-file/alternator/inspector)

(def layered-method make-alternatives ((component source-file/alternator/inspector) (class standard-class) (prototype asdf:source-file) (value asdf:source-file))
  (list* (make-instance 'source-file/lisp-form-list/inspector :component-value value)
         (call-next-layered-method)))

;;;;;;
;;; source-file/lisp-form-list/inspector

(def (component e) source-file/lisp-form-list/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component source-file/lisp-form-list/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-file-into-string (asdf:component-pathname component-value))))))

;;;;;;
;;; t/filter

(def method slot-type (class (prototype asdf:component) slot)
  (case (slot-definition-name slot)
    (asdf::name 'string)
    (t (call-next-method))))

(def layered-method map-filter-input ((component t/filter) (class standard-class) (prototype asdf:source-file) (value standard-class) function)
  (labels ((recurse (module)
             (iter (for component :in (asdf:module-components module))
                   (typecase component
                     (asdf:module (recurse component))
                     (asdf:source-file (funcall function component))))))
    (iter (for (system-name (version . system)) :in-hashtable asdf::*defined-systems*)
          (recurse system))))
