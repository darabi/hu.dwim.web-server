;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; model/abstract

(def (component e) model/abstract (component-value/mixin)
  ())

;;;;;;
;;; model/minimal

(def (component e) model/minimal (model/abstract component/minimal)
  ())

;;;;;;
;;; model/basic

(def (component e) model/basic (model/minimal component/basic)
  ())

;;;;;;
;;; model/style

(def (component e) model/style (model/basic component/style)
  ())

;;;;;;
;;; model/full

(def (component e) model/full (model/style component/full)
  ())
