;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Line chart

(def (component e) line-chart (chart)
  ())

(def render-xhtml line-chart
  (render-chart -self- "amline" ))
