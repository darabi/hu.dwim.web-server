;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Viewer abstract

(def (component e) viewer/abstract (component-value/mixin)
  ())

;;;;;;
;;; Viewer minimal

(def (component e) viewer/minimal (viewer/abstract component/minimal)
  ())

;;;;;;
;;; Viewer basic

(def (component e) viewer/basic (viewer/minimal component/basic)
  ())

;;;;;;
;;; Viewer style

(def (component e) viewer/style (viewer/basic component/style)
  ())

;;;;;;
;;; Viewer full

(def (component e) viewer/full (viewer/style component/full)
  ())
