;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; table/abstract

(def special-variable *table*)

(def (component e) table/abstract ()
  ())

(def component-environment table/abstract
  (bind ((*table* -self-))
    (call-next-method)))
