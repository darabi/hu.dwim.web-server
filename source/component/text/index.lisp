;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; index/inspector

(def (component e) index/inspector ()
  ())

(def (macro e) index/inspector ()
  `(make-instance 'index/inspector))

(def render-xhtml index/inspector
  (not-yet-implemented))
