;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; page-navigation-bar/mixin

(def (component e) page-navigation-bar/mixin ()
  ((page-navigation-bar :type component))
  (:documentation "A COMPONENT with a PAGE-NAVIGATION-BAR."))

(def (function e) render-page-navigation-bar-for (component)
  (render-component (page-navigation-bar-of component)))
