;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; table/component

(def special-variable *table*)

(def (component e) table/component ()
  ())

(def component-environment table/component
  (bind ((*table* -self-))
    (call-next-method)))
