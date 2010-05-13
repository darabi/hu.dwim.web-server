;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; line/chart

(def (component e) line/chart (standard/chart)
  ())

(def render-xhtml line/chart
  (render-chart -self- "amline"))
