;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Filter abstract

(def (component e) filter/abstract (component-value/mixin)
  ())

;;;;;;
;;; Filter minimal

(def (component e) filter/minimal (filter/abstract component/minimal)
  ())

;;;;;;
;;; Filter basic

(def (component e) filter/basic (filter/minimal component/basic)
  ())

;;;;;;
;;; Filter style

(def (component e) filter/style (filter/basic component/style)
  ())

;;;;;;
;;; Filter full

(def (component e) filter/full (filter/style component/full)
  ())
