;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component-class/alternator/inspector

(def (component e) component-class/alternator/inspector (class/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null component-class) component-class/alternator/inspector)

(def layered-method make-alternatives ((component component-class/alternator/inspector) (class standard-class) (prototype component-class) (value component-class))
  (list* (make-instance 'component-class/documentation/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; component-class/documentation/inspector

(def (component e) component-class/documentation/inspector (t/documentation/inspector)
  ())
