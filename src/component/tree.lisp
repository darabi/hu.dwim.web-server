;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree

(def special-variable *tree*)

(def special-variable *tree-level*)

(def component tree-component (remote-identity-component-mixin)
  ((columns nil :type components)
   (expander-column-index 0 :type integer)
   (root-nodes nil :type components)
   (expand-nodes-by-default #f :type boolean)))

(def call-in-component-environment tree-component ()
  (bind ((*tree* -self-)
         (*tree-level* -1))
    (call-next-method)))

(def render tree-component ()
  (bind (((:read-only-slots root-nodes id) -self-))
    <table (:id ,id :class "tree")
      <thead <tr ,(render-tree-columns -self-) >>
      <tbody ,(foreach #'render root-nodes) >>))

(def render-csv tree-component ()
  (render-csv-line (columns-of -self-))
  (render-csv-line-separator)
  (foreach #'render-csv (root-nodes-of -self-)))

(def (layered-function e) render-tree-columns (tree-component)
  (:method ((self tree-component))
    (foreach #'render (columns-of self))))

;;;;;;
;;; Node

(def component node-component (remote-identity-component-mixin style-component-mixin)
  ((child-nodes nil :type components)
   (cells nil :type components)))

(def layered-method render-onclick-handler ((self node-component))
  nil)

(def render node-component ()
  (bind (((:read-only-slots child-nodes expanded id style) -self-)
         (tree-id (id-of *tree*)))
    <tr (:id ,id :class ,(tree-node-style-class -self-) :style ,style
         :onclick ,(render-onclick-handler -self-)
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,tree-id ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,tree-id ,id))
      ,(render-tree-node-cells -self-) >
    (when expanded
      (foreach #'render child-nodes))))

(def render-csv node-component ()
  (render-csv-line (cells-of -self-))
  (render-csv-line-separator)
  (foreach #'render-csv (child-nodes-of -self-)))

(def (layered-function e) tree-node-style-class (component)
  (:method ((self node-component))
    (concatenate-string "level-" (integer-to-string *tree-level*) " " (css-class-of self))))

(def (function e) render-tree-node-expander (node-component)
  (with-slots (child-nodes expanded) node-component
    (if child-nodes
        <a (:href ,(action/href () (setf expanded (not expanded))))
           <img (:src ,(concatenate-string (path-prefix-of *application*)
                                           (if expanded
                                               "static/wui/icons/20x20/arrowhead-down.png"
                                               "static/wui/icons/20x20/arrowhead-right.png")))>>
        <span (:class "non-expandable")>)))

(def (function e) render-tree-node-expander-cell (node-component)
  (with-slots (cells) node-component
    <td (:class "expander")
        ,(render-tree-node-expander node-component)
        ,(bind ((expander-cell (elt cells (expander-column-index-of *tree*))))
           (if (stringp expander-cell)
               (render expander-cell)
               (progn
                 (ensure-uptodate expander-cell)
                 (render (content-of expander-cell)))))>))

(def (layered-function e) render-tree-node-cells (node-component)
  (:method ((self node-component))
    (iter (with expander-column-index = (expander-column-index-of *tree*))
          (for index :from 0)
          (for cell :in (cells-of self))
          (if (= index expander-column-index)
              (render-tree-node-expander-cell self)
              (render cell)))))

(def call-in-component-environment node-component ()
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))

;;;;;;
;;; Entire node

(def component entire-node-component (remote-identity-component-mixin content-component)
  ())

(def function render-entire-node (tree node body-thunk)
  (bind (((:read-only-slots id) node)
         (tree-id (id-of tree)))
    (list <tr (:id ,id)
              <td (:colspan ,(length (columns-of tree))
                   :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,tree-id ,id)
                   :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,tree-id ,id))
                  ,(funcall body-thunk)>>)))

(def layered-method render-onclick-handler ((self entire-node-component))
  nil)

(def render entire-node-component ()
  (render-entire-node *tree* -self- #'call-next-method))
