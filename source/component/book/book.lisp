;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; book/inspector

(def (component e) book/inspector (t/inspector exportable/abstract)
  ())

(def (macro e) book/inspector ((&rest args &key &allow-other-keys) &body book)
  `(make-instance 'book/inspector ,@args :component-value ,(the-only-element book)))

(def layered-method find-inspector-type-for-prototype ((prototype book))
  'book/inspector)

(def layered-method make-alternatives ((component book/inspector) class prototype (value book))
  (list* (delay-alternative-component-with-initargs 'book/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/text/inspector

(def (component e) t/text/inspector (inspector/style contents/widget)
  ())

(def refresh-component t/text/inspector
  (bind (((:slots contents component-value) -self-))
    (setf contents (mapcar [make-value-inspector !1 :initial-alternative-type 't/text/inspector]
                           (contents-of component-value)))))

(def render-text t/text/inspector
  (iter (for content :in (contents-of -self-))
        (write-text-line-begin)
        (render-component content)
        (write-text-line-separator)))

(def method render-command-bar-for-alternative? ((component t/text/inspector))
  #f)

;;;;;;
;;; book/text/inspector

(def (component e) book/text/inspector (t/text/inspector collapsible/abstract title/mixin)
  ())

(def render-xhtml book/text/inspector
  (with-render-style/abstract (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-title-for -self-)
    (when (expanded-component? -self-)
      (render-contents-for -self-))))

(def render-text book/text/inspector
  (write-text-line-begin)
  (render-title-for -self-)
  (write-text-line-separator)
  (call-next-method))

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

  (:method ((self component))
    (render-component self)))
