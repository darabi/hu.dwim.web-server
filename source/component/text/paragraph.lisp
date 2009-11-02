;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; paragraph/inspector

(def (component e) paragraph/inspector (text/inspector)
  ())

(def (macro e) paragraph/inspector ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'paragraph/inspector ,@args :contents (list ,@contents)))

(def layered-method find-inspector-type-for-prototype ((prototype paragraph))
  'paragraph/inspector)

(def layered-method make-alternatives ((component paragraph/inspector) class prototype (value paragraph))
  (list* (delay-alternative-component-with-initargs 'paragraph/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; paragraph/text/inspector

(def (component e) paragraph/text/inspector (t/text/inspector)
  ())

(def refresh-component paragraph/text/inspector)

(def render-xhtml paragraph/text/inspector
  (with-render-style/abstract (-self- :element-name "p")
    (render-contents-for -self-)))

(def layered-function render-paragraph (component)
  (:method :in xhtml-layer ((self number))
    <p ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <p ,(render-component self)>)

  (:method ((self component))
    (render-component self)))
