;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/inspector

(def (component e) component/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null component) component/inspector)

(def layered-method make-alternatives ((component component/inspector) (class component-class) (prototype component) (value component))
  (list* (make-instance 'component/render-xhtml-output/inspector :component-value value)
         (make-instance 'component/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; component/documentation/inspector

(def (component e) component/documentation/inspector (t/documentation/inspector)
  ())

;;;;;;
;;; component/render-xhtml-output/inspector

(def (component e) component/render-xhtml-output/inspector (inspector/basic quote-xml-string-content/widget)
  ())

(def refresh-component component/render-xhtml-output/inspector
  (setf (content-of -self-) (string-trim-whitespace (render-to-xhtml-string (component-value-of -self-)))))
