;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Inspector abstract

(def (component e) inspector/abstract (editable/mixin component-value/mixin)
  ())

;;;;;;
;;; Inspector minimal

(def (component e) inspector/minimal (inspector/abstract component/minimal)
  ())

;;;;;;
;;; Inspector basic

(def (component e) inspector/basic (inspector/minimal component/basic)
  ())

;;;;;;
;;; Inspector style

(def (component e) inspector/style (inspector/basic component/style)
  ())

;;;;;;
;;; Inspector full

(def (component e) inspector/full (inspector/style component/full)
  ())
