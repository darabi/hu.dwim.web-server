;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; finder/abstract

(def (component e) finder/abstract (component-value/mixin)
  ())

;;;;;;
;;; finder/minimal

(def (component e) finder/minimal (finder/abstract component/minimal)
  ())

;;;;;;
;;; finder/basic

(def (component e) finder/basic (finder/minimal component/basic)
  ())

;;;;;;
;;; finder/style

(def (component e) finder/style (finder/basic component/style)
  ())

;;;;;;
;;; finder/full

(def (component e) finder/full (finder/style component/full)
  ())

;;;;;;
;;; Finder factory

(def layered-method make-finder (type &rest args &key &allow-other-keys)
  (not-yet-implemented))
