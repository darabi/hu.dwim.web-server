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

(def subtype-mapper *inspector-type-mapping* (or null standard-generic-function) standard-generic-function/inspector)

(def layered-method make-alternatives ((component standard-generic-function/inspector) (class funcallable-standard-class) (prototype standard-generic-function) (value standard-generic-function))
  (list* (delay-alternative-component-with-initargs 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods value))
         (call-next-method)))
