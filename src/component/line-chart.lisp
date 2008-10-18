;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Line chart

(def component line-chart (chart)
  ())

(def render line-chart ()
  (render-chart -self- "amline" ))
