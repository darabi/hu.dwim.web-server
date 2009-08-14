;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; footer/mixin

(def (component e) footer/mixin ()
  ((footer :type component))
  (:documentation "A COMPONENT with a FOOTER."))

(def (function e) render-footer (component)
  (render-component (footer-of component)))
