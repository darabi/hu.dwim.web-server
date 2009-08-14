;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; toc/viewer

(def (component e) toc/viewer ()
  ())

(def (macro e) toc/viewer ()
  `(make-instance 'toc/viewer))

(def render-xhtml toc/viewer
  (not-yet-implemented))
