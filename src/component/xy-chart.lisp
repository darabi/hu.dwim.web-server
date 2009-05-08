;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Xy chart

(def component xy-chart (chart)
  ())

(def render-xhtml xy-chart
  (render-chart -self- "amxy"))
