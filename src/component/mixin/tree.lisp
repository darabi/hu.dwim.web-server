;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree abstract

(def special-variable *tree*)

(def special-variable *tree-level*)

(def (component e) tree/abstract ()
  ())

(def component-environment tree/abstract
  (bind ((*tree* -self-)
         (*tree-level* -1))
    (call-next-method)))

;;;;;;
;;; Node abstract

(def (component e) node/abstract ()
  ())

(def component-environment node/abstract
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))
