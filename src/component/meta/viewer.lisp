;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; viewer/abstract

(def (component e) viewer/abstract (component-value/mixin)
  ())

;;;;;;
;;; viewer/minimal

(def (component e) viewer/minimal (viewer/abstract component/minimal)
  ())

;;;;;;
;;; viewer/basic

(def (component e) viewer/basic (viewer/minimal component/basic)
  ())

;;;;;;
;;; viewer/style

(def (component e) viewer/style (viewer/basic component/style)
  ())

;;;;;;
;;; viewer/full

(def (component e) viewer/full (viewer/style component/full)
  ())

;;;;;;
;;; Viewer factory

(def layered-method make-viewer (type value &rest args &key &allow-other-keys)
  (apply #'make-instance type value :editable #f :edited #f args))
