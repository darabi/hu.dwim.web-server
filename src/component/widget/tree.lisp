;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree widget

(def (component e) tree/widget (tree/abstract
                                widget/basic
                                collapsible/mixin
                                selection/mixin
                                frame-unique-id/mixin)
  ((root-nodes nil :type components))
  (:documentation "TODO: collapsible, resizable, scrolling and selection"))

(def (macro e) tree/widget ((&rest args &key &allow-other-keys) &body root-nodes)
  `(make-instance 'tree/widget ,@args :root-nodes (list ,@root-nodes)))

;; TODO: move this non widgetness to the viewer/inspector etc.
(def refresh-component tree/widget
  (bind (((:slots root-nodes) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (ensure-list (component-value-of -self-))))
    (if root-nodes
        (foreach [setf (component-value-of !1) !2] root-nodes component-value)
        (setf root-nodes (mapcar [make-tree/root-node -self- dispatch-class dispatch-prototype !1] component-value)))))

(def render-xhtml tree/widget
  (bind (((:read-only-slots id root-nodes) -self-))
    <div (:id ,id :class "tree widget")
      ,(foreach #'render-component root-nodes)>))

(def (generic e) find-tree/parent (component class prototype value)
  ;; KLUDGE: to allow instantiating widget without componente-value
  (:method ((component component) class prototype value)
    nil))

(def (generic e) collect-tree/children (component class prototype value)
  ;; KLUDGE: to allow instantiating widget without componente-value
  (:method ((component component) (class null) (prototype null) (value null))
    (make-list (length (root-nodes-of component)) :initial-element nil)))

(def (generic e) make-tree/root-node (component class prototype value))

;;;;;;
;;; Node widget

(def (component e) node/widget (node/abstract
                                widget/basic
                                content/abstract
                                context-menu/mixin
                                collapsible/mixin
                                selectable/mixin
                                frame-unique-id/mixin)
  ((child-nodes nil :type components)))

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
    <div (:id ,id :class `str("node widget level-" ,(integer-to-string *tree-level*)))
      ,(render-context-menu-for -self-)
      <span (:class `str("content " ,(selectable-component-style-class -self-)))
            ,(when child-nodes
               (render-component (make-toggle-expanded-command -self-)))
            ,(render-content-for -self-)>
      ,(when (expanded-component? -self-)
         (foreach #'render-component child-nodes))>
    (render-command-onclick-handler (find-command -self- 'select-component) id)))

(def (generic e) make-node/content (component class prototype value))

(def (generic e) make-node/child-node (component class prototype value))

(def method collect-tree/children ((component component) (class null) (prototype null) (value null))
  ;; KLUDGE: to allow instantiating widget without componente-value
  (make-list (length (child-nodes-of component)) :initial-element nil))

(def layered-method make-context-menu-items ((component node/widget) class prototype value)
  (optional-list* (make-menu-item (make-select-component-command component class prototype value) nil)
                  (call-next-method)))
