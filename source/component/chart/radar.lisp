;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; radar/chart

(def (component e) radar/chart (chart/abstract)
  ())

(def render-xhtml radar/chart
  (render-chart -self- "amradar"))
