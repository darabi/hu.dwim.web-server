;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Radar chart

(def component radar-chart (chart)
  ())

(def render-xhtml radar-chart
  (render-chart -self- "amradar"))
