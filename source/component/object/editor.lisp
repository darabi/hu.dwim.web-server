;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/editor

(def (component e) t/editor (t/presentation)
  ())

;;;;;;
;;; t/editor

(def (component e) t/editor (t/presentation
                             cloneable/component
                             layer/mixin)
  ())
