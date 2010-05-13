;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; uri/alternator/inspector

(def (component e) uri/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null uri) uri/alternator/inspector)

(def layered-method make-alternatives ((component uri/alternator/inspector) (class standard-class) (prototype uri) (value uri))
  ;; TOOD: not all uris are external links
  (list* (make-instance 'uri/external-link/inspector :component-value value) (call-next-layered-method)))

;;;;;;
;;; uri/external-link/inspector

(def (component e) uri/external-link/inspector (t/detail/inspector)
  ())

(def render-xhtml uri/external-link/inspector
  (with-render-style/component (-self- :element-name "span")
    (bind ((uri (print-uri-to-string (component-value-of -self-))))
      ;; TODO: refactor this to use the external-link/widget
      <a (:href ,uri :target "_blank")
        ,uri ,(render-component (icon/widget external-link))>)))

(def render-text uri/external-link/inspector
  (render-component (print-uri-to-string (component-value-of -self-))))

(def render-ods uri/external-link/inspector
  (let ((uri (print-uri-to-string (component-value-of -self-))))
    <text:p <text:a (xlink:href ,uri) ,uri>>))

(def method render-command-bar-for-alternative? ((component uri/external-link/inspector))
  #f)
