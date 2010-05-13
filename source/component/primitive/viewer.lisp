;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/viewer

(def (component e) primitive/viewer (primitive/presentation t/viewer)
  ()
  (:documentation "A PRIMITIVE/VIEWER displays existing values of primitive TYPEs."))
