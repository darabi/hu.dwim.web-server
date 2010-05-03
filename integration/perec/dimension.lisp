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
         (call-next-layered-method)))

;;;;;;
;;; dimension/documentation/inspector

(def (component e) dimension/documentation/inspector (t/documentation/inspector title/mixin)
  ())

(def method make-documentation ((component dimension/documentation/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (hu.dwim.perec::documentation-of value))

(def render-component dimension/documentation/inspector
  (render-title-for -self-)
  (render-contents-for -self-))

(def render-xhtml dimension/documentation/inspector
  (with-render-style/abstract (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)))

(def layered-method make-title ((self dimension/documentation/inspector) (class standard-class) (prototype hu.dwim.perec::dimension) (value hu.dwim.perec::dimension))
  (title/widget ()
    (localized-dimension-name value :capitalize-first-letter #t)))
