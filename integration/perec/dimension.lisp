;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/inspector

(def layered-method make-alternatives ((component t/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (list* (make-instance 'dimension/documentation/inspector
                        :component-value value
                        :component-value-type (component-value-type-of component))
         (call-next-method)))

;;;;;;
;;; dimension/documentation/inspector

(def (component e) dimension/documentation/inspector (t/documentation/inspector)
  ())

(def method make-documentation ((component dimension/documentation/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (hu.dwim.perec::documentation-of value))
