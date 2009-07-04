;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree layout

(def (component e) tree/layout (tree/abstract layout/minimal root-nodes/mixin)
  ())

(def (macro e) tree/layout ((&rest args &key &allow-other-keys) &body root-nodes)
  `(make-instance 'tree/layout ,@args :root-nodes (list ,@root-nodes)))

(def render-xhtml tree/layout
  (bind (((:read-only-slots root-nodes) -self-))
    <div (:class "tree layout")
      ,(foreach #'render-component root-nodes)>))
