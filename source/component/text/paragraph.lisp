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

(def subtype-mapper *inspector-type-mapping* (or null paragraph) paragraph/inspector)

(def layered-method make-alternatives ((component paragraph/inspector) (class standard-class) (prototype paragraph) (value paragraph))
  (list* (make-instance 'paragraph/text/inspector :component-value value)
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

(def render-odt paragraph/text/inspector
  <text:p ,(render-contents-for -self-)>)

(def render-ods paragraph/text/inspector
  <table:table-row
    <table:table-cell (office:value-type "string")
      ,(foreach #'render-ods (contents-of -self-))>>)
