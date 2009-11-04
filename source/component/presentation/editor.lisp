;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; editor/abstract

(def (component e) editor/abstract (presentation/abstract)
  ())

;;;;;;
;;; editor/minimal

(def (component e) editor/minimal (editor/abstract presentation/minimal)
  ())

;;;;;;
;;; editor/basic

(def (component e) editor/basic (editor/minimal presentation/basic)
  ())

;;;;;;
;;; editor/style

(def (component e) editor/style (editor/basic presentation/style)
  ())

;;;;;;
;;; editor/full

(def (component e) editor/full (editor/style presentation/full)
  ())

;;;;;;
;;; Editor factory

(def layered-method make-value-editor (value &rest args)
  (apply #'make-editor (class-of value) value args))

(def layered-method make-editor (type value &rest args &key &allow-other-keys)
  (apply #'make-inspector type value :editable #f :edited #t args))
