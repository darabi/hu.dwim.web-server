;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; chapter/inspector

(def (component e) chapter/inspector (t/inspector)
  ())

(def (macro e) chapter/inspector ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter/inspector ,@args :contents (list ,@contents)))

(def layered-method find-inspector-type-for-prototype ((prototype chapter))
  'chapter/inspector)

(def layered-method make-alternatives ((component chapter/inspector) class prototype (value chapter))
  (list* (delay-alternative-component-with-initargs 'chapter/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; chapter/text/inspector

(def (component e) chapter/text/inspector (t/text/inspector title/mixin)
  ())


(def render-xhtml chapter/text/inspector
  (with-render-style/abstract (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)))

(def layered-method make-title ((self chapter/text/inspector) class prototype (value chapter))
  (title-of value))
