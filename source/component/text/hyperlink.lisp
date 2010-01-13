;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; hyperlink/inspector

(def (component e) hyperlink/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null hyperlink) hyperlink/inspector)

(def layered-method make-alternatives ((component hyperlink/inspector) (class standard-class) (prototype hyperlink) (value hyperlink))
  (list* (delay-alternative-component-with-initargs 'hyperlink/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; hyperlink/text/inspector

(def (component e) hyperlink/text/inspector (inspector/style t/detail/inspector content/widget)
  ())

(def refresh-component hyperlink/text/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-value-inspector (content-of component-value)))))

(def render-xhtml hyperlink/text/inspector
  (bind (((:read-only-slots component-value) -self-))
    <a (:class "external-link widget" :target "_blank" :href ,(print-uri-to-string (uri-of component-value)))
      ,(render-content-for -self-)
      ,(render-component (icon external-link))>))

(def method render-command-bar-for-alternative? ((component hyperlink/text/inspector))
  #f)
