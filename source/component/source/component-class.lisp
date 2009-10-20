;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component-class/inspector

(def (component e) component-class/inspector (t/inspector)
  ())

(def (macro e) component-class/inspector (component-class &rest args &key &allow-other-keys)
  `(make-instance 'component-class/inspector ,@args :component-value ,component-class))

(def layered-method find-inspector-type-for-prototype ((prototype component-class))
  'component-class/inspector)

(def layered-method make-alternatives ((component component-class/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'component-class/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; component-class/documentation/inspector

(def (component e) component-class/documentation/inspector (class/documentation/inspector)
  ())
