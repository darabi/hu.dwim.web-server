;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lisp-form-list/repl/inspector

(def (component e) lisp-form-list/repl/inspector (sequence/alternator/inspector)
  ())

(def layered-method make-element-presentation ((component lisp-form-list/repl/inspector) class prototype value)
  (not-yet-implemented))
