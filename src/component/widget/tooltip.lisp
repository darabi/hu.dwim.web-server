;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tooltip widget

(def (component e) tooltip/widget (content/mixin)
  ()
  (:documentation "A COMPONENT which pops up as a tooltip of another COMPONENT."))

(def render-component tooltip/widget
  (not-yet-implemented))
