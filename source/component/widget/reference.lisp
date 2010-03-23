;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; reference/widget

(def (component e) reference/widget (command/widget)
  ())

;; TODO: KLUDGE: command/widget is not rendered in passive-layer, but we want to be able to see static references
(def layered-method render-component :in passive-layer :around ((self reference/widget))
  (ensure-refreshed self)
  (render-content-for self))
