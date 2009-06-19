;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Radar chart

(def (component e) radar-chart (chart)
  ())

(def render-xhtml radar-chart
  (render-chart -self- "amradar"))
