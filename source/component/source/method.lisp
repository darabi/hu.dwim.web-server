;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-method/inspector

(def (component e) standard-method/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null standard-method) standard-method/inspector)

(def layered-method make-alternatives ((component standard-method/inspector) (class standard-class) (prototype standard-method) (value standard-method))
  (list* (delay-alternative-component-with-initargs 'standard-method/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; standard-method/lisp-form/inspector

(def (component e) standard-method/lisp-form/inspector (inspector/basic content/widget)
  ())

(def refresh-component standard-method/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source component-value)))))

;;;;;;
;;; standard-method-sequence/lisp-form-list/inspector

(def (component e) standard-method-sequence/lisp-form-list/inspector (sequence/list/inspector)
  ())

(def layered-method make-list/element ((component standard-method-sequence/lisp-form-list/inspector) class prototype value)
  (make-instance 'standard-method/lisp-form/inspector :component-value value))
