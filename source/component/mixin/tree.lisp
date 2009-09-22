;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tree/abstract

(def special-variable *tree*)

(def special-variable *tree-level*)

(def (component e) tree/abstract ()
  ())

(def component-environment tree/abstract
  (bind ((*tree* -self-)
         (*tree-level* -1))
    (call-next-method)))