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
   ;; TODO expander-column-index should be marked by a special column type, or something similar. this way it's very fragile...
   (expander-column-index 0 :type integer)
   (root-nodes nil :type components)
   (expand-nodes-by-default #f :type boolean)))

(def component-environment tree-component
  (bind ((*tree* -self-)
         (*tree-level* -1))
    (call-next-method)))

(def render-xhtml tree-component
  (bind (((:read-only-slots root-nodes id) -self-))
    <table (:id ,id :class "tree")
      <thead <tr ,(render-tree-columns -self-) >>
      <tbody ,(foreach #'render root-nodes) >>))

(def render-csv tree-component
  (render-csv-line (columns-of -self-))
  (render-csv-line-separator)
  (foreach #'render (root-nodes-of -self-)))

(def render-ods tree-component
  <table:table
    <table:table-row ,(foreach #'render (columns-of -self-))>
    ,(foreach #'render (root-nodes-of -self-))>)

(def (layered-function e) render-tree-columns (tree-component)
  (:method ((self tree-component))
    (foreach #'render (columns-of self))))

;;;;;;
;;; Node

(def component node-component (remote-identity-component-mixin style-component-mixin)
  ((child-nodes nil :type components)
   (cells nil :type components)))

(def render-xhtml node-component
  (bind (((:read-only-slots child-nodes expanded id style) -self-)
         (tree-id (id-of *tree*))
         (onclick-handler? (render-onclick-handler -self-)))
    <tr (:id ,id :style ,style :class ,(concatenate-string (tree-node-style-class -self-) (when onclick-handler? " selectable"))
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,tree-id ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,tree-id ,id))
      ,(render-tree-node-cells -self-) >
    (when expanded
      (foreach #'render child-nodes))))

(def render-csv node-component
  (render-csv-line (cells-of -self-))
  (render-csv-line-separator)
  (foreach #'render (child-nodes-of -self-)))

(def render-ods node-component
  <table:table-row ,(foreach (lambda (column cell)
                               (render-ods-tree-cell *tree* -self- column cell))
                             (columns-of *tree*)
                             (cells-of -self-))>
  (awhen (child-nodes-of -self-)
    <table:table-row-group ,(foreach #'render (child-nodes-of -self-))>))

(def layered-method render-onclick-handler ((self node-component))
  nil)

(def (layered-function e) tree-node-style-class (component)
  (:method ((self node-component))
    (concatenate-string "level-" (integer-to-string *tree-level*) " " (css-class-of self))))

(def (function e) render-tree-node-expander (node-component)
  (bind (((:slots child-nodes expanded) node-component)
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
                                                        (mark-dirty tree))
                                                     :event event
                                                     :ajax ,(when (ajax-enabled? *application*)
                                                              (id-of tree)))))))
        <span (:class "non-expandable")>)))

(def (function e) render-tree-node-expander-cell (node-component)
  (bind (((:read-only-slots cells) node-component)
         (expander-cell (elt cells (expander-column-index-of *tree*))))
    (render-cell-component (expander-cell :css-class "expander")
      (render-tree-node-expander node-component)
      (if (stringp expander-cell)
          (render expander-cell)
          (progn
            (ensure-uptodate expander-cell)
            (render (content-of expander-cell)))))))

(def (layered-function e) render-tree-node-cells (node-component)
  (:method ((self node-component))
    (iter (with expander-column-index = (expander-column-index-of *tree*))
          (for index :from 0)
          (for cell :in (cells-of self))
          (for column :in (columns-of *tree*))
          (when (force (visible-p column))
            (if (= index expander-column-index)
                (render-tree-node-expander-cell self)
                (render-tree-cell *tree* self column cell))))))

(def component-environment node-component
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))

(def (layered-function e) render-tree-cell (tree node column cell)
  (:method :before ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((tree tree-component) (node node-component) (column column-component) (cell component))
    <td ,(render cell)>)

  (:method ((tree tree-component) (node node-component) (column column-component) (cell string))
    <td ,(render cell)>)
  
  (:method ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (render cell)))

(def (layered-function e) render-ods-tree-cell (tree node column cell)
  (:method :before ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
           (ensure-uptodate cell))

  (:method ((tree tree-component) (node node-component) (column column-component) (cell component))
    <table:table-cell ,(render cell)>)

  (:method ((tree tree-component) (node node-component) (column column-component) (cell string))
    <table:table-cell ,(render cell)>)

  (:method ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (render cell)))

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

(def render-xhtml entire-node-component
  (render-entire-node *tree* -self- #'call-next-method))
