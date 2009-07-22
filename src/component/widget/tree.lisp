;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree widget

(def (component e) tree/widget (tree/abstract
                                widget/style
                                root-nodes/mixin
                                collapsible/mixin
                                selection/mixin)
  ()
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
  (bind (((:read-only-slots root-nodes) -self-))
    (with-render-style/abstract (-self-)
      (foreach #'render-component root-nodes))))

;; TODO: kill, we don't need the parent
(def (generic e) find-tree/parent (component class prototype value)
  ;; KLUDGE: to allow instantiating widget without componente-value
  ;; TODO: remove it
  (:method ((component component) class prototype value)
    nil))

;; TODO: move to mixin
(def (generic e) collect-tree/children (component class prototype value)
  ;; KLUDGE: to allow instantiating widget without componente-value
  ;; TODO: remove it
  (:method ((component tree/widget) (class null) (prototype null) (value null))
    (make-list (length (root-nodes-of component)) :initial-element nil)))

(def (generic e) make-tree/root-node (component class prototype value))
