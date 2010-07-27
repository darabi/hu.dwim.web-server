;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; chapter/alternator/inspector

(def (component e) chapter/alternator/inspector (text/alternator/inspector exportable/component)
  ())

(def subtype-mapper *inspector-type-mapping* (or null chapter) chapter/alternator/inspector)

(def layered-method make-alternatives ((component chapter/alternator/inspector) (class standard-class) (prototype chapter) (value chapter))
  (list* (make-instance 'chapter/text/inspector :component-value value)
         (make-instance 'chapter/toc/inspector :component-value value)
         (call-next-layered-method)))

(def method component-style-class ((self chapter/alternator/inspector))
  (%component-style-class self))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) (prototype chapter) (value chapter))
  (string+ (toc-numbering component) " " (title-of value)))

;;;;;;
;;; chapter/text/inspector

(def (special-variable e) *chapter-level* 0)

(def (component e) chapter/text/inspector (t/text/inspector
                                           collapsible-contents/component
                                           title/mixin)
  ())

(def component-environment chapter/text/inspector
  (bind ((*chapter-level* (1+ *chapter-level*)))
    (call-next-method)))

(def render-xhtml chapter/text/inspector
  (with-render-style/component (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-title-for -self-)
    (when (expanded-component? -self-)
      <div (:class "separator") <br>>
      <div (:class "content")
        ,(render-contents-for -self-)>)))

(def render-odt chapter/text/inspector
  <text:h (text:style-name `str("Heading." ,(integer-to-string *chapter-level*)))
    ,(render-title-for -self-)>
  (render-contents-for -self-))

(def render-text chapter/text/inspector
  (write-text-line-begin)
  (bind ((position (file-position *text-stream*)))
    (render-title-for -self-)
    (write-text-line-separator)
    (write-text-line-begin)
    (write-characters #\- (- (file-position *text-stream*) position 1) *text-stream*))
  (write-text-line-separator)
  (call-next-layered-method))

(def layered-method make-title ((self chapter/text/inspector) class prototype (value chapter))
  (title-of value))

;;;;;;
;;; ods export
(def render-ods chapter/text/inspector
  <table:table-row
    <table:table-cell (office:value-type "string")
      <text:p ,(title-of (component-value-of -self-))>>>
  (foreach #'render-ods (contents-of -self-)))
