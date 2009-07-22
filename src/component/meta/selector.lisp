;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; selector/abstract

(def (component e) selector/abstract (component-value/mixin)
  ())

;;;;;;
;;; selector/minimal

(def (component e) selector/minimal (selector/abstract component/minimal)
  ())

;;;;;;
;;; selector/basic

(def (component e) selector/basic (selector/minimal component/basic)
  ())

;;;;;;
;;; selector/style

(def (component e) selector/style (selector/basic component/style)
  ())

;;;;;;
;;; selector/full

(def (component e) selector/full (selector/style component/full)
  ())

;;;;;;
;;; Selector factory

(def layered-method make-selector (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
