;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lazy/mixin

(def (component e) lazy/mixin ()
  ((lazily-rendered-component
    #t
    :type boolean))
  (:documentation "TODO"))

(def render-component :in xhtml-layer :around lazy/mixin
  (ensure-refreshed -self-)
  (if (lazily-rendered-component? -self-)
      (render-component-stub -self-)
      (call-next-layered-method)))
