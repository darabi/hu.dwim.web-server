;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; book/inspector

(def (component e) book/inspector (t/inspector)
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

;;;;;;
;;; book/text/inspector

(def (component e) book/text/inspector (t/text/inspector title/mixin)
  ())

(def render-xhtml book/text/inspector
  (with-render-style/abstract (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)))

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
