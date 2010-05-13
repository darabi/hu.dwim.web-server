;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; column/component

(def special-variable *column-index*)

(def (component e) column/component ()
  ())

(def method supports-debug-component-hierarchy? ((self column/component))
  #f)

;;;;;;
;;; columns/mixin

(def (component e) columns/mixin ()
  ((columns :type components))
  (:documentation "A COMPONENT with a SEQUENCE of COLUMNs."))

(def (function e) render-columns-for (component)
  (foreach #'render-component (columns-of component)))
