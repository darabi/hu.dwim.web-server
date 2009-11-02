;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; external-link/widget

;; TODO: make this a subclass of command/widget and forget about this url stuff, that will be part of uri/external-link/inspector
(def (component e) external-link/widget (widget/basic content/abstract)
  ((url :type string)))

(def (macro e) external-link/widget ((&rest args &key &allow-other-keys) &body url-and-content)
  (assert (length= 2 url-and-content))
  `(make-instance 'external-link/widget ,@args :url ,(first url-and-content) :content ,(second url-and-content)))

(def render-xhtml external-link/widget
  (bind (((:read-only-slots url) -self-))
    <a (:class "external-link widget" :target "_blank" :href ,url)
      ,(render-content-for -self-)>))
