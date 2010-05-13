;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; row/layout

(def (component e) row/layout (standard/layout row/component cells/mixin)
  ((vertical-alignment nil :type (member nil :top :center :bottom))))

(def (macro e) row/layout ((&rest args &key &allow-other-keys) &body cells)
  `(make-instance 'row/layout ,@args :cells (list ,@cells)))

(def render-xhtml row/layout
  <tr (:class "row layout")
    ,(render-cells-for -self-)>)
