;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; cell/layout

(def (component e) cell/layout (standard/layout content/component)
  ((column-span nil :type integer)
   (row-span nil :type integer)
   (horizontal-alignment nil :type (member nil :left :center :right))
   (vertical-alignment nil :type (member nil :top :center :bottom))))

(def (macro e) cell/layout ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'cell/layout ,@args :content ,(the-only-element content)))

(def render-xhtml cell/layout
  ;; TODO: alignment, span
  <td (:class "cell layout")
    ,(render-content-for -self-)>)
