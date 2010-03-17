;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; chapter/inspector

(def (component e) chapter/inspector (text/inspector exportable/abstract)
  ())

(def subtype-mapper *inspector-type-mapping* (or null chapter) chapter/inspector)

(def layered-method make-alternatives ((component chapter/inspector) (class standard-class) (prototype chapter) (value chapter))
  (list* (make-instance 'chapter/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; chapter/text/inspector

(def (component e) chapter/text/inspector (t/text/inspector collapsible/abstract title/mixin)
  ())

(def render-xhtml chapter/text/inspector
  (with-render-style/abstract (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-title-for -self-)
    <div (:class "title-separator") <br>>
    (when (expanded-component? -self-)
      <div (:class "content")
        ,(render-contents-for -self-)>)))

(def render-odt chapter/text/inspector
  <text:p ,(render-title-for -self-)
          ,(render-contents-for -self-)>)

(def render-text chapter/text/inspector
  (write-text-line-begin)
  (render-title-for -self-)
  (write-text-line-separator)
  (call-next-method))

(def layered-method make-title ((self chapter/text/inspector) class prototype (value chapter))
  (title-of value))

;;;;;;
;;; ods export
(def render-ods chapter/text/inspector
    <table:table-row
      <table:table-cell (office:value-type "string")
                        <text:p ,(title-of (component-value-of -self-)) >>>
  (foreach #'render-ods (contents-of -self-)))
