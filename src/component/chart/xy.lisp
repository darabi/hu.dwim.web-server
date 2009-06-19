;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; XY chart

(def (component e) xy/chart (chart/abstract)
  ())

(def render-xhtml xy/chart
  (render-chart -self- "amxy"))
