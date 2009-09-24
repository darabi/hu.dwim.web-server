;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-generic-function/inspector

(def (component e) standard-generic-function/inspector (t/inspector)
  ())

(def (macro e) standard-generic-function/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'standard-generic-function/inspector ,@args :component-value ,(the-only-element name)))

(def layered-method find-inspector-type-for-prototype ((prototype standard-generic-function))
  'standard-generic-function/inspector)

(def layered-method make-alternatives ((component standard-generic-function/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods value))
         (call-next-method)))
