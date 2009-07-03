;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Model abstract

(def (component e) model/abstract (component-value/mixin)
  ())

;;;;;;
;;; Model minimal

(def (component e) model/minimal (model/abstract component/minimal)
  ())

;;;;;;
;;; Model basic

(def (component e) model/basic (model/minimal component/basic)
  ())

;;;;;;
;;; Model style

(def (component e) model/style (model/basic component/style)
  ())

;;;;;;
;;; Model full

(def (component e) model/full (model/style component/full)
  ())
