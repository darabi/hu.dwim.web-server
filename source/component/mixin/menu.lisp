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

(def render-component :before menu-items/mixin
  (sort-menu-items -self-))

(def (generic e) sort-menu-items (component)
  (:method :around ((self menu-items/mixin))
    (setf (menu-items-of self) (call-next-method)))

  (:method ((self menu-items/mixin))
    (foreach #'ensure-refreshed (menu-items-of self))
    (sort (menu-items-of self) #'< :key #'command-position)))
