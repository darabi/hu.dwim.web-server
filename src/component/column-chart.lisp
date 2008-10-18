;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Column chart

(def component column-chart (chart)
  ())

(def render column-chart ()
  (render-chart -self- "amcolumn" ))

