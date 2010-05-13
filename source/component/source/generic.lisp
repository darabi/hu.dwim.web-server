;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-generic-function/alternator/inspector

(def (component e) standard-generic-function/alternator/inspector (function/alternator/inspector)
  ())

;; KLUDGE: closer-mop:standard-generic-function is a subclass of common-lisp:standard-generic-function and thus generic function are not instances of it
(def subtype-mapper *inspector-type-mapping* (or null common-lisp:standard-generic-function) standard-generic-function/alternator/inspector)

;; KLUDGE: both prototype and value supposed to be of type standard-generic-function
(def layered-method make-alternatives ((component standard-generic-function/alternator/inspector) (class funcallable-standard-class) (prototype function) (value common-lisp:standard-generic-function))
  (list* (make-instance 'function/documentation/inspector :component-value value)
         (make-instance 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods value))
         (call-next-layered-method)))
