;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Definition

(def (class* e) definition ()
  ((name :type symbol)
   (documentation :type string)))

(def (class* e) special-variable-definition (definition)
  ())

(def (class* e) macro-definition (definition)
  ())

(def (class* e) function-definition (definition)
  ())

(def (class* e) generic-function-definition (definition)
  ())

(def (class* e) class-definition (definition)
  ())
