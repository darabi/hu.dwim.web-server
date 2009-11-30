;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component-class/inspector

(def (component e) component-class/inspector (class/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null component-class) component-class/inspector)

(def layered-method make-alternatives ((component component-class/inspector) (class standard-class) (prototype component-class) (value component-class))
  (list* (delay-alternative-component-with-initargs 'component-class/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; component-class/documentation/inspector

(def (component e) component-class/documentation/inspector (t/documentation/inspector)
  ())
