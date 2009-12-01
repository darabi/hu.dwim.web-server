;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; primitive/viewer

(def (component e) primitive/viewer (primitive/presentation viewer/abstract)
  ()
  (:documentation "A PRIMITIVE/INSPECTOR displays existing values of primitive TYPEs."))
