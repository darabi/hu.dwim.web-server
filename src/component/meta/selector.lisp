;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Selector abstract

(def (component e) selector/abstract (component-value/mixin)
  ())

;;;;;;
;;; Selector minimal

(def (component e) selector/minimal (selector/abstract component/minimal)
  ())

;;;;;;
;;; Selector basic

(def (component e) selector/basic (selector/minimal component/basic)
  ())

;;;;;;
;;; Selector style

(def (component e) selector/style (selector/basic component/style)
  ())

;;;;;;
;;; Selector full

(def (component e) selector/full (selector/style component/full)
  ())
