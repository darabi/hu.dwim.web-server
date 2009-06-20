;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tooltip basic

(def (component e) tooltip/basic (content/mixin)
  ())

(def render-component tooltip/basic
  (render-tooltip -self-))
