;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-method/alternator/inspector

(def (component e) standard-method/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null standard-method) standard-method/alternator/inspector)

(def layered-method make-alternatives ((component standard-method/alternator/inspector) (class standard-class) (prototype standard-method) (value standard-method))
  (list* (make-instance 'standard-method/lisp-form/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; standard-method/lisp-form/inspector

(def (component e) standard-method/lisp-form/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component standard-method/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (definition-source-text component-value)))))

;;;;;;
;;; standard-method-sequence/lisp-form-list/inspector

(def (component e) standard-method-sequence/lisp-form-list/inspector (sequence/list/inspector)
  ())

(def layered-method make-element-presentation ((component standard-method-sequence/lisp-form-list/inspector) class prototype value)
  (make-instance 'standard-method/lisp-form/inspector :component-value value))
