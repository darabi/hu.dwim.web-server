;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Commands mixin

(def (component e) commands/mixin (menu-bar/mixin context-menu/mixin command-bar/mixin)
  ()
  (:documentation "A COMPONENT with various COMPONENTs providing behaviour through COMMANDs."))
