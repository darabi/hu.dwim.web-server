;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree widget

(def (component e) tree/widget (tree/abstract
                                widget/style
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

(def render-xhtml tree/widget
  (bind (((:read-only-slots root-nodes) -self-))
    (with-render-style/abstract (-self-)
      (foreach #'render-component root-nodes))))
