;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; deep-arguments/mixin

(def (component e) deep-arguments/mixin ()
  ((deep-arguments
    nil
    :type list
    :documentation "A list of arguments destructured during building the component hierarchy."))
  (:documentation "A component that supports providing arguments for descendant components down the hierarchy."))

(def (function e) component-deep-arguments (component name)
  (getf (deep-arguments-of component) name))
