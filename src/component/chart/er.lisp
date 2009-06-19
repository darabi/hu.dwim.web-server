;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Entity relationship chart

(def (component e) entity-relationship/chart (chart/abstract)
  ())

(def render-xhtml entity-relationship/chart
  (not-yet-implemented))
