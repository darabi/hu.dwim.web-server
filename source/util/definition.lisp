;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Definition

(def class* definition ()
  ((name :type symbol)))

(def class* special-variable-definition (definition)
  ())

(def class* macro-definition (definition)
  ())

(def class* function-definition (definition)
  ())

(def class* generic-function-definition (definition)
  ())
