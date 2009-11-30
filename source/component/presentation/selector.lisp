;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; selector/abstract

(def (component e) selector/abstract (presentation/abstract)
  ())

;;;;;;
;;; selector/minimal

(def (component e) selector/minimal (selector/abstract presentation/minimal)
  ())

;;;;;;
;;; selector/basic

(def (component e) selector/basic (selector/minimal presentation/basic)
  ())

;;;;;;
;;; selector/style

(def (component e) selector/style (selector/basic presentation/style)
  ())

;;;;;;
;;; selector/full

(def (component e) selector/full (selector/style presentation/full)
  ())

;;;;;;
;;; Selector factory

(def layered-method make-selector (type &rest args &key &allow-other-keys)
  (declare (ignore type args))
  (not-yet-implemented))
