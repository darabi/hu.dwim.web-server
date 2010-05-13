;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; hyperlink/alternator/inspector

(def (component e) hyperlink/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null hyperlink) hyperlink/alternator/inspector)

(def method component-style-class ((self hyperlink/alternator/inspector))
  "hyperlink inspector")

(def layered-method make-alternatives ((component hyperlink/alternator/inspector) (class standard-class) (prototype hyperlink) (value hyperlink))
  (list* (make-instance 'hyperlink/text/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; hyperlink/text/inspector

(def (component e) hyperlink/text/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component hyperlink/text/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (aif (content-of component-value)
                       (make-value-inspector it
                                             :edited (edited-component? -self-)
                                             :editable (editable-component? -self-))
                       (print-uri-to-string (uri-of component-value))))))

(def render-xhtml hyperlink/text/inspector
  (bind (((:read-only-slots component-value) -self-))
    (with-render-style/component (-self- :element-name "span")
      <a (:class "external-link widget" :target "_blank" :href ,(print-uri-to-string (uri-of component-value)))
         ,(render-content-for -self-)
         ,(render-component (icon/widget external-link))>)))

(def render-odt hyperlink/text/inspector
  (bind (((:read-only-slots component-value) -self-))
    <text:a (;;xlink:type "simple"
             ;;office:name "link name"
             xlink:href ,(print-uri-to-string (uri-of component-value)))
      ,(render-content-for -self-)
      ;; TODO icon
      >))

(def render-text hyperlink/text/inspector
  (render-content-for -self-)
  (bind (((:read-only-slots component-value) -self-))
    (when component-value
      (write-string " (" *text-stream*)
      (write-string (print-uri-to-string (uri-of component-value)) *text-stream*)
      (write-char #\) *text-stream*))))

(def method render-command-bar-for-alternative? ((component hyperlink/text/inspector))
  #f)
