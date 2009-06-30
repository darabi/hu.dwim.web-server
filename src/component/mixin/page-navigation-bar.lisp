;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Page navigation bar mixin

(def (component e) page-navigation-bar/mixin ()
  ((page-navigation-bar :type component))
  (:documentation "A COMPONENT with a PAGE-NAVIGATION-BAR."))
