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
   (package :type package)
   (documentation :type (or null string))
   (source-file :type (or null pathname))))

(def (class* e) constant-definition (definition)
  ())

(def (class* e) variable-definition (definition)
  ())

(def (class* e) macro-definition (definition)
  ())

(def (class* e) function-definition (definition)
  ())

(def (class* e) generic-function-definition (definition)
  ())

(def (class* e) type-definition (definition)
  ())

(def (class* e) structure-definition (definition)
  ())

(def (class* e) condition-definition (definition)
  ())

(def (class* e) class-definition (definition)
  ())

(def (class* e) package-definition (definition)
  ())
