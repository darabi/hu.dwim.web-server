;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; nodrow/layout

(def (component e) nodrow/layout (standard/layout node/component cells/mixin child-nodes/mixin)
  ())

(def (macro e) nodrow/layout ((&rest args &key &allow-other-keys) &body child-nodes)
  `(make-instance 'nodrow/layout ,@args :child-nodes (list ,@child-nodes)))

(def render-xhtml nodrow/layout
  (bind (((:read-only-slots child-nodes) -self-))
    <tr (:class `str("nodrow layout level-" ,(integer-to-string *tree-level*)))
      ,(render-cells-for -self-)>
    (foreach #'render-component child-nodes)))
