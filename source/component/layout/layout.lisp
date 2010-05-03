;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; layout/abstract

(def (component e) layout/abstract ()
  ()
  (:documentation "A LAYOUT/ABSTRACT is an arrangement of its child COMPONENTs, it does not provide behaviour on the client side to modify its state. It might have various visual appearance properties to control the look and feel."))

;;;;;;
;;; layout/minimal

(def (component e) layout/minimal (layout/abstract component/minimal)
  ())

;;;;;;
;;; layout/basic

(def (component e) layout/basic (layout/minimal component/basic)
  ())

;;;;;;
;;; layout/style

(def (component e) layout/style (layout/basic component/style)
  ())
