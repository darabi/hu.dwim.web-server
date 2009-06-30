;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Menu items mixin

(def (component e) menu-items/mixin ()
  ((menu-items nil :type components))
  (:documentation "A COMPONENT with a set of MENU-ITEMs."))
