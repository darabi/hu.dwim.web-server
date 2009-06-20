;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu bar mixin

(def (component e) menu-bar/mixin ()
  ((menu-bar :type component))
  (:documentation "A COMPONENT with a MENU-BAR."))
