;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

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

;; TODO: this is kind of unknown what do want here
(def layered-method make-invoker (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
