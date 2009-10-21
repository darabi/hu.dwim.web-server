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

(def refresh-component page-navigation-bar/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (page-navigation-bar-of -self-) (make-page-navigation-bar -self- class prototype value))))

(def (layered-function e) make-page-navigation-bar (component class prototype value))

(def (function e) render-page-navigation-bar-for (component)
  (render-component (page-navigation-bar-of component)))
