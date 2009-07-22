;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; maker/abstract

(def (component e) maker/abstract (component-value/mixin)
  ())

;;;;;;
;;; maker/minimal

(def (component e) maker/minimal (maker/abstract component/minimal)
  ())

;;;;;;
;;; maker/basic

(def (component e) maker/basic (maker/minimal component/basic)
  ())

;;;;;;
;;; maker/style

(def (component e) maker/style (maker/basic component/style)
  ())

;;;;;;
;;; maker/full

(def (component e) maker/full (maker/style component/full)
  ())

;;;;;;
;;; Maker factory

(def layered-method make-maker (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
