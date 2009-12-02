;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; invoker/abstract

(def (component e) invoker/abstract (presentation/abstract)
  ())

;;;;;;
;;; invoker/minimal

(def (component e) invoker/minimal (invoker/abstract presentation/minimal)
  ())

;;;;;;
;;; invoker/basic

(def (component e) invoker/basic (invoker/minimal presentation/basic)
  ())

;;;;;;
;;; invoker/style

(def (component e) invoker/style (invoker/basic presentation/style)
  ())

;;;;;;
;;; invoker/full

(def (component e) invoker/full (invoker/style presentation/full)
  ())
