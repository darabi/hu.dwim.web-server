;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; node/widget

(def (component e) node/widget (node/abstract
                                collapsible/abstract
                                widget/basic
                                content/abstract
                                child-nodes/mixin
                                context-menu/mixin
                                collapsible/mixin
                                selectable/mixin
                                frame-unique-id/mixin)
  ())

(def (macro e) node/widget ((&rest args &key &allow-other-keys) content &body child-nodes)
  `(make-instance 'node/widget ,@args :content ,content :child-nodes (list ,@child-nodes)))

(def render-xhtml node/widget
  (bind (((:read-only-slots id child-nodes) -self-))
    <div (:id ,id :class `str("node widget level-" ,(integer-to-string *tree-level*))
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      <span (:class `str("content " ,(selectable-component-style-class -self-)))
        ,(when child-nodes
           (render-collapse-or-expand-command-for -self-))
        ,(render-content-for -self-)>
      ,(when (expanded-component? -self-)
         (foreach #'render-component child-nodes))>
    (render-command-onclick-handler (find-command -self- 'select-component) id)))

(def method visible-child-component-slots ((self node/widget))
  (remove-slots (append (unless (expanded-component? self)
                          '(child-nodes))
                        (unless (child-nodes-of self)
                          '(collapse-command expand-command)))
                (call-next-method)))
