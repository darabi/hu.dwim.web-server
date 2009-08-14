;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; column/abstract

(def special-variable *column-index*)

(def (component e) column/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self column/abstract))
  #f)

;;;;;;
;;; columns/mixin

(def (component e) columns/mixin ()
  ((columns :type components))
  (:documentation "A COMPONENT with a SEQUENCE of COLUMNs."))

(def (function e) render-columns-for (component)
  (foreach #'render-component (columns-of component)))
