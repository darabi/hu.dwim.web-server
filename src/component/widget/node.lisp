;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Node widget

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

;; TODO: move this non widgetness to the viewer/inspector etc.
(def refresh-component node/widget
  (bind (((:slots child-nodes content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-tree/children -self- dispatch-class dispatch-prototype component-value)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-node/content -self- dispatch-class dispatch-prototype component-value)))
    (if child-nodes
        (foreach [setf (component-value-of !1) !2] child-nodes children)
        (setf child-nodes (mapcar [make-node/child-node -self- dispatch-class dispatch-prototype !1] children)))))

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

(def (generic e) make-node/content (component class prototype value))

(def (generic e) make-node/child-node (component class prototype value))

(def method collect-tree/children ((component node/widget) (class null) (prototype null) (value null))
  ;; KLUDGE: to allow instantiating widget without component-value
  ;; TODO: kill this
  (make-list (length (child-nodes-of component)) :initial-element nil))
