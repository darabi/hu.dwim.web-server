;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/viewer

(def (component e) t/viewer (viewer/basic
                             t/presentation
                             cloneable/abstract
                             layer/mixin)
  ())
