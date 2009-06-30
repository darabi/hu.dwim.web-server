;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Editor abstract

(def (component e) editor/abstract (component-value/mixin)
  ())

(def method editable-component? ((self editor/abstract))
  #t)

;;;;;;
;;; Editor minimal

(def (component e) editor/minimal (editor/abstract component/minimal)
  ())

;;;;;;
;;; Editor basic

(def (component e) editor/basic (editor/minimal component/basic)
  ())

;;;;;;
;;; Editor style

(def (component e) editor/style (editor/basic component/style)
  ())

;;;;;;
;;; Editor full

(def (component e) editor/full (editor/style component/full)
  ())
