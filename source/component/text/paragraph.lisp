;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; paragraph/alternator/inspector

(def (component e) paragraph/alternator/inspector (text/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null paragraph) paragraph/alternator/inspector)

(def layered-method make-alternatives ((component paragraph/alternator/inspector) (class standard-class) (prototype paragraph) (value paragraph))
  (list* (make-instance 'paragraph/text/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; paragraph/text/inspector

(def (component e) paragraph/text/inspector (t/text/inspector)
  ())

(def refresh-component paragraph/text/inspector)

(def render-text paragraph/text/inspector
  (write-text-line-begin)
  (call-next-layered-method)
  (write-text-line-separator)
  (write-text-line-separator))

(def render-xhtml paragraph/text/inspector
  (with-render-style/component (-self- :element-name "p")
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
