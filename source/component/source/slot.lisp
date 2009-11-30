;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-slot-definition/inspector

(def (component e) standard-slot-definition/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null standard-slot-definition) standard-slot-definition/inspector)

(def layered-method make-alternatives ((component standard-slot-definition/inspector) (class standard-class) (prototype standard-slot-definition) (value standard-slot-definition))
  (list* (delay-alternative-component-with-initargs 'standard-slot-definition/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; standard-slot-definition/lisp-form/inspector

(def (component e) standard-slot-definition/lisp-form/inspector ()
  ())
