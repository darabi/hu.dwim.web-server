;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Maker abstract

(def (component e) maker/abstract (component-value/mixin)
  ())

;;;;;;
;;; Maker minimal

(def (component e) maker/minimal (maker/abstract component/minimal)
  ())

;;;;;;
;;; Maker basic

(def (component e) maker/basic (maker/minimal component/basic)
  ())

;;;;;;
;;; Maker style

(def (component e) maker/style (maker/basic component/style)
  ())

;;;;;;
;;; Maker full

(def (component e) maker/full (maker/style component/full)
  ())
