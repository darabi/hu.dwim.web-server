;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; uri/inspector

(def (component e) uri/inspector (t/inspector)
  ())

(def layered-method find-inspector-type-for-prototype ((prototype uri))
  'uri/inspector)

(def layered-method make-alternatives ((component uri/inspector) class prototype (value uri))
  (list* (delay-alternative-component-with-initargs 'uri/external-link/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; uri/external-link/inspector

(def (component e) uri/external-link/inspector (inspector/style)
  ())

(def render-xhtml uri/external-link/inspector
  (with-render-style/abstract (-self-)
    (bind ((uri (print-uri-to-string (component-value-of -self-))))
      <a (:href ,uri :target "_new") ,uri>)))

(def method render-command-bar-for-alternative? ((component uri/external-link/inspector))
  #f)
