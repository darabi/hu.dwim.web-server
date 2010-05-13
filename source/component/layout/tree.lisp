;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tree/layout

(def (component e) tree/layout (standard/layout tree/component root-nodes/mixin)
  ())

(def (macro e) tree/layout ((&rest args &key &allow-other-keys) &body root-nodes)
  `(make-instance 'tree/layout ,@args :root-nodes (list ,@root-nodes)))

(def render-xhtml tree/layout
  (bind (((:read-only-slots root-nodes) -self-))
    <div (:class "tree layout")
      ,(foreach #'render-component root-nodes)>))
