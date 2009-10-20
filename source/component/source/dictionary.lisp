;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; dictionary/inspector

(def (component e) dictionary/inspector (t/inspector)
  ())

(def (macro e) dictionary/inspector (dictionary &rest args &key &allow-other-keys)
  `(make-instance 'dictionary/inspector ,@args :component-value ,dictionary))

(def layered-method find-inspector-type-for-prototype ((prototype dictionary))
  'dictionary/inspector)

(def layered-method make-alternatives ((component dictionary/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'dictionary/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; dictionary/documentation/inspector

(def (component e) dictionary/documentation/inspector (t/documentation/inspector)
  ())
