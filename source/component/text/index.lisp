;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; index/alternator/inspector

(def (component e) index/alternator/inspector (t/alternator/inspector)
  ())

(def render-xhtml index/alternator/inspector
  (not-yet-implemented))
