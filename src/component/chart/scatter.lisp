;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Scatter chart

(def (component e) scatter/chart (chart/abstract)
  ())

(def render-xhtml scatter/chart
  (render-chart -self- "amxy"))