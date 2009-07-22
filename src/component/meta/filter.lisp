;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; filter/abstract

(def (component e) filter/abstract (component-value/mixin)
  ())

;;;;;;
;;; filter/minimal

(def (component e) filter/minimal (filter/abstract component/minimal)
  ())

;;;;;;
;;; filter/basic

(def (component e) filter/basic (filter/minimal component/basic)
  ())

;;;;;;
;;; filter/style

(def (component e) filter/style (filter/basic component/style)
  ())

;;;;;;
;;; filter/full

(def (component e) filter/full (filter/style component/full)
  ())

;;;;;;
;;; Filter factory

(def layered-method make-filter (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
