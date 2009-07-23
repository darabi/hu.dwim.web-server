;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; node/abstract

(def (component e) node/abstract ()
  ())

(def component-environment node/abstract
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))

;;;;;;
;;; root-nodes/mixin

(def (component e) root-nodes/mixin ()
  ((root-nodes nil :type components)))

;;;;;;
;;; child-nodes/mixin

(def (component e) child-nodes/mixin ()
  ((child-nodes nil :type components)))
