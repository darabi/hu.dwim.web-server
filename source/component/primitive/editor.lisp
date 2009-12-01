;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/editor

(def (component e) primitive/editor (primitive/presentation editor/abstract)
  ()
  (:documentation "A PRIMITIVE/INSPECTOR edits existing values of primitive TYPEs."))
