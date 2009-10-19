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

(def (macro e) component/inspector (component &rest args &key &allow-other-keys)
  `(make-instance 'component/inspector ,@args :component-value ,component))

(def layered-method find-inspector-type-for-prototype ((prototype component))
  'component/inspector)

(def layered-method make-alternatives ((component component/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'component/render-xhtml-output/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; component/render-xhtml-output/inspector

(def (component e) component/render-xhtml-output/inspector (inspector/basic quote-xml-string-content/widget)
  ())

(def refresh-component component/render-xhtml-output/inspector
  (setf (content-of -self-) (string-trim-whitespace (render-to-xhtml-string (component-value-of -self-)))))
