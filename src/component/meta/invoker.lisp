;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Invoker abstract

(def (component e) invoker/abstract (component-value/mixin)
  ())

;;;;;;
;;; Invoker minimal

(def (component e) invoker/minimal (invoker/abstract component/minimal)
  ())

;;;;;;
;;; Invoker basic

(def (component e) invoker/basic (invoker/minimal component/basic)
  ())

;;;;;;
;;; Invoker style

(def (component e) invoker/style (invoker/basic component/style)
  ())

;;;;;;
;;; Invoker full

(def (component e) invoker/full (invoker/style component/full)
  ())
