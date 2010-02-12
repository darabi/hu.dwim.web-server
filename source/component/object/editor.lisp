;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/editor

(def (component e) t/editor (editor/basic
                             t/presentation
                             cloneable/abstract
                             layer/mixin)
  ())
