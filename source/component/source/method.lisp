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

(def (macro e) standard-method/inspector ((&rest args &key &allow-other-keys) &body method)
  `(make-instance 'standard-method/inspector ,@args :component-value ,(the-only-element method)))

(def layered-method find-inspector-type-for-prototype ((prototype standard-method))
  'standard-method/inspector)

(def layered-method make-alternatives ((component standard-method/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'standard-method/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; standard-method/lisp-form/inspector

(def (component e) standard-method/lisp-form/inspector (inspector/basic content/widget)
  ())

(def (macro e) standard-method/lisp-form/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'standard-method/lisp-form/inspector ,@args :component-value ,(the-only-element name)))

(def refresh-component standard-method/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source component-value)))))

;;;;;;
;;; standard-method-sequence/lisp-form-list/inspector

(def (component e) standard-method-sequence/lisp-form-list/inspector (sequence/list/inspector)
  ())

(def layered-method make-list/element ((component standard-method-sequence/lisp-form-list/inspector) class prototype value)
  (make-instance 'standard-method/lisp-form/inspector :component-value value))
