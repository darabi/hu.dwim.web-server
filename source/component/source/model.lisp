;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

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
