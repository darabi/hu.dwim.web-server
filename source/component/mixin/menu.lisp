;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;
;;; menu-items/mixin

(def (component e) menu-items/mixin ()
  ((menu-items nil :type components))
  (:documentation "A COMPONENT with a set of MENU-ITEMs."))
