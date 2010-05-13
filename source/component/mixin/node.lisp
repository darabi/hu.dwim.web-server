;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; node/component

(def (component e) node/component ()
  ())

(def component-environment node/component
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
