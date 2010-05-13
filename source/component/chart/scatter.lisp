;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; scatter/chart

(def (component e) scatter/chart (standard/chart)
  ())

(def render-xhtml scatter/chart
  (render-chart -self- "amxy"))
