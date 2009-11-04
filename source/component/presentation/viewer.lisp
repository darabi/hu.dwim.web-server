;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; viewer/abstract

(def (component e) viewer/abstract (presentation/abstract)
  ())

;;;;;;
;;; viewer/minimal

(def (component e) viewer/minimal (viewer/abstract presentation/minimal)
  ())

;;;;;;
;;; viewer/basic

(def (component e) viewer/basic (viewer/minimal presentation/basic)
  ())

;;;;;;
;;; viewer/style

(def (component e) viewer/style (viewer/basic presentation/style)
  ())

;;;;;;
;;; viewer/full

(def (component e) viewer/full (viewer/style presentation/full)
  ())

;;;;;;
;;; Viewer factory

(def layered-method make-value-viewer (value &rest args)
  (apply #'make-viewer (class-of value) value args))

(def layered-method make-viewer (type value &rest args &key &allow-other-keys)
  (apply #'make-inspector type value :editable #f :edited #f args))
