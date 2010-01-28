;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; package/inspector

(def (component e) package/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null package) package/inspector)

(def layered-method make-alternatives ((component package/inspector) (class standard-class) (prototype package) (value package))
  (list* (make-instance 'package/definition-sequence/inspector :component-value value)
         (call-next-layered-method)))

;;;;;;
;;; package/definition-sequence/inspector

(def (component e) package/definition-sequence/inspector (inspector/style)
  ())
