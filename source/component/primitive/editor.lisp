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
  (:documentation "A PRIMITIVE/EDITOR edits existing values of primitive TYPEs."))
