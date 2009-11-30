;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; presentation/abstract

(def (component e) presentation/abstract (component-value/mixin component-value-type/mixin)
  ())

;;;;;;
;;; presentation/minimal

(def (component e) presentation/minimal (presentation/abstract component/minimal)
  ())

;;;;;;
;;; presentation/basic

(def (component e) presentation/basic (presentation/minimal component/basic)
  ())

;;;;;;
;;; presentation/style

(def (component e) presentation/style (presentation/basic component/style)
  ())

;;;;;;
;;; presentation/full

(def (component e) presentation/full (presentation/style component/full)
  ())
