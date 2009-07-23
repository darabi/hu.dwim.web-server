;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; line/chart

(def (component e) line/chart (chart/abstract)
  ())

(def render-xhtml line/chart
  (render-chart -self- "amline"))
