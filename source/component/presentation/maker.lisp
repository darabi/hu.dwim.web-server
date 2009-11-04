;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; maker/abstract

(def (component e) maker/abstract (presentation/abstract)
  ())

;;;;;;
;;; maker/minimal

(def (component e) maker/minimal (maker/abstract presentation/minimal)
  ())

;;;;;;
;;; maker/basic

(def (component e) maker/basic (maker/minimal presentation/basic)
  ())

;;;;;;
;;; maker/style

(def (component e) maker/style (maker/basic presentation/style)
  ())

;;;;;;
;;; maker/full

(def (component e) maker/full (maker/style presentation/full)
  ())

;;;;;;
;;; Maker factory

(def layered-method make-maker (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
