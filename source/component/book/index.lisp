;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; index/viewer

(def (component e) index/viewer ()
  ())

(def (macro e) index/viewer ()
  `(make-instance 'index/viewer))

(def render-xhtml index/viewer
  (not-yet-implemented))
