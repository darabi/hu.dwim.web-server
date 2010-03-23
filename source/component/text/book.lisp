;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; book/inspector

(def (icon e) book)

(def (component e) book/inspector (text/inspector exportable/abstract)
  ())

(def subtype-mapper *inspector-type-mapping* (or null book) book/inspector)

(def layered-method make-alternatives ((component book/inspector) (class standard-class) (prototype book) (value book))
  (list* (make-instance 'book/text/inspector :component-value value)
         (call-next-method)))

(def layered-method export-file-name (format (component book/inspector) (value book))
  (title-of value))

;;;;;;
;;; book/text/inspector

(def (component e) book/text/inspector (t/text/inspector collapsible/abstract title/mixin exportable/abstract)
  ())

(def render-xhtml book/text/inspector
  (with-render-style/abstract (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-title-for -self-)
    (foreach #'render-author (authors-of (component-value-of -self-)))
    <div (:class "separator") <br>>
    (when (expanded-component? -self-)
      <div (:class "content")
        ,(render-contents-for -self-)>)))

(def render-odt book/text/inspector
  <text:p ,(render-title-for -self-)>
  (awhen (authors-of (component-value-of -self-))
    <text:p ,(foreach #'render-author it)>)
  (render-contents-for -self-))

(def render-text book/text/inspector
  (write-text-line-begin)
  (bind ((position (file-position *text-stream*)))
    (render-title-for -self-)
    (write-text-line-separator)
    (write-text-line-begin)
    (write-characters #\= (- (file-position *text-stream*) position 1) *text-stream*))
  (write-text-line-separator)
  (write-text-line-begin)
  (foreach #'render-author (authors-of (component-value-of -self-)))
  (write-text-line-separator)
  (call-next-method))

(def render-ods book/text/inspector
  (render-title-for -self-)
  (foreach #'render-author (authors-of (component-value-of -self-)))
  (foreach #'render-component (contents-of -self-)))

(def layered-method make-title ((self book/text/inspector) class prototype (value book))
  (title-of value))

;;;;;;
;;; Author

(def (layered-function e) render-author (component)
  (:method :in xhtml-layer ((self number))
    <div (:class "author")
      ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <div (:class "author")
      ,(render-component self)>)

  (:method ((self string))
    (render-component self))

  (:method ((self component))
    (render-component self)))

;;;;;;
;;; book/tree-level/inspector

(def (component e) book/tree-level/inspector (t/tree-level/inspector)
  ())

(def layered-method make-tree-level/path ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value list))
  (make-instance 't/tree-level/path/inspector :component-value value))

(def layered-method make-path/content ((component t/tree-level/path/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 't/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/previous-sibling ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 't/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/next-sibling ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) value)
  (make-instance 't/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/descendants ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 't/tree-level/tree/inspector :component-value value))

(def layered-method make-tree-level/node ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (make-instance 't/tree-level/reference/inspector :component-value value))

(def layered-method collect-tree/children ((component book/tree-level/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (collect-if (of-type 'title-mixin) (contents-of value)))

(def layered-method collect-tree/children ((component t/node/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (collect-if (of-type 'title-mixin) (contents-of value)))

(def layered-method make-reference-content ((component t/tree-level/reference/inspector) (class standard-class) (prototype hu.dwim.wui::title-mixin) (value hu.dwim.wui::title-mixin))
  (title-of value))
