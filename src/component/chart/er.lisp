;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Diagram basic

(def component diagram/basic ()
  ())

;;;;;;
;;; Icon

(def icon diagram)

;;;;;;
;;; Localization

(def resources hu
  (icon-label.diagram "Ábra")
  (icon-tooltip.diagram "Ábra megjelenítése"))

(def resources en
  (icon-label.diagram "Diagram")
  (icon-tooltip.diagram "Show diagram"))
