;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tree/widget

(def (component e) tree/widget (tree/component
                                standard/widget
                                root-nodes/mixin
                                selection/mixin
                                command-bar/mixin
                                context-menu/mixin
                                resizable/mixin
                                scrollable/mixin
                                collapsible/mixin)
  ()
  (:documentation "TODO: collapsible, resizable, scrolling and selection"))

(def (macro e) tree/widget ((&rest args &key &allow-other-keys) &body root-nodes)
  `(make-instance 'tree/widget ,@args :root-nodes (list ,@root-nodes)))

(def method component-style-class ((self tree/widget))
  (string+ "content-border " (call-next-method)))

(def render-xhtml tree/widget
  (bind (((:read-only-slots root-nodes) -self-))
    (with-render-style/component (-self-)
      (foreach #'render-component root-nodes)
      (render-command-bar-for -self-)
      (render-context-menu-for -self-))))
