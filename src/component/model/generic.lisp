;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Generic function inspector

(def (component e) generic-function/inspector ()
  ())

(def render-xhtml generic-function/inspector
  (not-yet-implemented))

;;;;;;
;;; Generic method inspector

(def (component e) generic-method/inspector ()
  ())

(def render-xhtml generic-method/inspector
  (not-yet-implemented))
