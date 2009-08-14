;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; header/mixin

(def (component e) header/mixin ()
  ((header :type component))
  (:documentation "A COMPONENT with a HEADER."))

(def (function e) render-header-for (component)
  (render-component (header-of component)))
