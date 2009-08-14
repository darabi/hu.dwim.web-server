;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; node/layout

(def (component e) node/layout (node/abstract layout/minimal content/abstract child-nodes/mixin)
  ())

(def (macro e) node/layout ((&rest args &key &allow-other-keys) content &body child-nodes)
  `(make-instance 'node/layout ,@args :content ,content :child-nodes (list ,@child-nodes)))

(def render-xhtml node/layout
  (bind (((:read-only-slots child-nodes) -self-))
    <div (:class `str("node layout level-" ,(integer-to-string *tree-level*)))
      <span (:class "content")
        ,(render-content-for -self-)>
      ,(foreach #'render-component child-nodes)>))
