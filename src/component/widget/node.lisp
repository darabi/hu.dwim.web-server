;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Node abstract

(def (component e) node/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self node/abstract))
  #f)

;;;;;;
;;; Node widget

(def (component e) node/widget (style/abstract cells/mixin selectable/mixin)
  ((child-nodes nil :type components)))

(def render-xhtml node/widget
  (bind (((:read-only-slots child-nodes expanded id style) -self-)
         (onclick-handler? (render-onclick-handler -self- :left)))
    <tr (:id ,id :style ,style :class ,(concatenate-string (tree-node-style-class -self-) (when onclick-handler? " selectable"))
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-tree-node-cells -self-) >
    (when expanded
      (foreach #'render-component child-nodes))))

(def render-csv node/widget
  (write-csv-line (cells-of -self-))
  (write-csv-line-separator)
  (foreach #'render-component (child-nodes-of -self-)))

(def render-ods node/widget
  <table:table-row ,(foreach (lambda (column cell)
                               (render-ods-tree-cell *tree* -self- column cell))
                             (columns-of *tree*)
                             (cells-of -self-))>
  (awhen (child-nodes-of -self-)
    <table:table-row-group ,(foreach #'render-component (child-nodes-of -self-))>))

(def layered-method render-onclick-handler ((self node/widget) (button (eql :left)))
  nil)

(def (layered-function e) tree-node-style-class (component)
  (:method ((self node/widget))
    (concatenate-string "level-" (integer-to-string *tree-level*) " " (css-class-of self))))

(def (function e) render-tree-node-expander (node)
  (bind (((:slots child-nodes expanded) node)
         (tree *tree*))
    (if child-nodes
        (bind ((id (generate-response-unique-string)))
          <img (:id ,id :src ,(concatenate-string (path-prefix-of *application*)
                                                  (if expanded
                                                      "static/wui/icons/20x20/arrowhead-down.png"
                                                      "static/wui/icons/20x20/arrowhead-right.png")))>
          `js(on-load (dojo.connect (dojo.by-id ,id) "onclick" nil
                                    (lambda (event)
                                      (wui.io.action ,(action/href ()
                                                        (setf expanded (not expanded))
                                                        ;; NOTE: we make dirty the whole tree, because it is difficult to replace the rows corresponding to the tree node
                                                        (mark-to-be-rendered tree))
                                                     :event event
                                                     :ajax ,(when (ajax-enabled? *application*)
                                                              (id-of tree)))))))
        <span (:class "non-expandable")>)))

(def (function e) render-tree-node-expander-cell (node)
  (bind ((expander-cell (elt (cells-of node) (expander-column-index-of *tree*))))
    (render-cell-component (expander-cell :css-class "expander")
      (render-tree-node-expander node)
      (if (stringp expander-cell)
          (render-component expander-cell)
          (progn
            (ensure-refreshed expander-cell)
            (render-component (content-of expander-cell)))))))

(def (layered-function e) render-tree-node-cells (component)
  (:method ((self node/widget))
    (iter (with expander-column-index = (expander-column-index-of *tree*))
          (for index :from 0)
          (for cell :in (cells-of self))
          (for column :in (columns-of *tree*))
          (when (visible-component? column)
            (if (= index expander-column-index)
                (render-tree-node-expander-cell self)
                (render-cells *tree* self column cell))))))

(def component-environment node/widget
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))

;;;;;;
;;; Entire node

(def (component e) entire-node/widget (id/mixin content/mixin)
  ())

(def function render-entire-node (tree node body-thunk)
  (bind (((:read-only-slots id) node))
    (list <tr (:id ,id)
              <td (:colspan ,(length (columns-of tree))
                   :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
                   :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
                  ,(funcall body-thunk)>>)))

(def layered-method render-onclick-handler ((self entire-node/widget) (button (eql :left)))
  nil)

(def render-xhtml entire-node/widget
  (render-entire-node *tree* -self- #'call-next-method))
