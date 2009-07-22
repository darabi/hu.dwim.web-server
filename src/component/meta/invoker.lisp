;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; invoker/abstract

(def (component e) invoker/abstract (component-value/mixin)
  ())

;;;;;;
;;; invoker/minimal

(def (component e) invoker/minimal (invoker/abstract component/minimal)
  ())

;;;;;;
;;; invoker/basic

(def (component e) invoker/basic (invoker/minimal component/basic)
  ())

;;;;;;
;;; invoker/style

(def (component e) invoker/style (invoker/basic component/style)
  ())

;;;;;;
;;; invoker/full

(def (component e) invoker/full (invoker/style component/full)
  ())

;;;;;;
;;; Invoker factory

(def layered-method make-invoker (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
