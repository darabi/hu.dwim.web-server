;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lisp-form-list/repl/inspector

(def (component e) lisp-form-list/repl/inspector (sequence/inspector)
  ())

(def method add-list-element ((component lisp-form-list/repl/inspector) class prototype value)
  (not-yet-implemented))

(def layered-method make-list/element ((component lisp-form-list/repl/inspector) class prototype value)
  (not-yet-implemented))
