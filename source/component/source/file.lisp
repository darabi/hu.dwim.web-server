;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; source-file/inspector

(def (component e) source-file/inspector (t/inspector)
  ())

(def (macro e) source-file/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'source-file/inspector ,@args :component-value ,(the-only-element file)))

(def layered-method find-inspector-type-for-prototype ((prototype asdf:source-file))
  'source-file/inspector)

(def layered-method make-alternatives ((component source-file/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'source-file/lisp-form-list/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; source-file/lisp-form-list/inspector

(def (component e) source-file/lisp-form-list/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def (macro e) source-file/lisp-form-list/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'source-file/lisp-form-list/inspector ,@args :component-value ,(the-only-element file)))

(def refresh-component source-file/lisp-form-list/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector
                                 :component-value (read-lisp-source (asdf:component-pathname component-value))))))

;;;;;;
;;; t/name-value-list/filter

(def layered-method map-filter-input ((component t/name-value-list/filter) (class standard-class) (prototype standard-class) (value (eql (find-class 'asdf:source-file))) function)
  (labels ((recurse (module)
             (iter (for component :in (asdf:module-components module))
                   (typecase component
                     (asdf:module (recurse component))
                     (asdf:source-file (funcall function component))))))
    (iter (for (system-name (version . system)) :in-hashtable asdf::*defined-systems*)
          (recurse system))))
