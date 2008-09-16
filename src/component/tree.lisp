;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree

(def special-variable *tree-level*)

(def component tree-component (remote-identity-component-mixin)
  ((columns nil :type components)
   (root-node nil :type component)
   (expand-nodes-by-default #f :type boolean)))

(def render tree-component ()
  (bind (((:read-only-slots root-node id) -self-))
    <table (:id ,id :class "tree")
      <thead <tr ,(render-tree-columns -self-)>>
      <tbody ,(render root-node)>>))

(def call-in-component-environment tree-component ()
  (bind ((*tree-level* -1))
    (call-next-method)))

(def (layered-function e) render-tree-columns (tree-component)
  (:method ((self tree-component))
    (map nil #'render (columns-of self))))

;;;;;;
;;; Node

(def component node-component (remote-identity-component-mixin style-component-mixin)
  ((child-nodes nil :type components)
   (cells nil :type components)))

(def render node-component ()
  (bind (((:read-only-slots child-nodes expanded id) -self-))
    <tr (:id ,id
         :style ,(style-of -self-)
         :class ,(tree-node-style-class -self-))
        ,(render-tree-node-expander-cell -self-)
        ,(render-tree-node-cells -self-) >
    (when expanded
      (map nil #'render child-nodes))))

(def (layered-function e) tree-node-style-class (component)
  (:method ((self node-component))
    (concatenate-string "level-" (integer-to-string *tree-level*) " " (css-class-of self))))

(def (function e) render-tree-node-expander (node-component)
  (with-slots (child-nodes expanded) node-component
    (if child-nodes
        <a (:href ,(make-action-href () (setf expanded (not expanded))))
           <img (:src ,(concatenate-string (path-prefix-of *application*)
                                           (if expanded
                                               "static/wui/icons/20x20/arrowhead-down.png"
                                               "static/wui/icons/20x20/arrowhead-right.png")))>>
        <span (:class "non-expandable")>)))

(def (function e) render-tree-node-expander-cell (node-component)
  (with-slots (cells) node-component
    <td (:class "expander")
        ,(render-tree-node-expander node-component)
        ,(bind ((first-cell (first cells)))
               (if (stringp first-cell)
                   `xml,first-cell
                   (progn
                     (ensure-uptodate first-cell)
                     (render (content-of first-cell))))) >))

(def (layered-function e) render-tree-node-cells (node-component)
  (:method ((self node-component))
    (map nil #'render (rest (cells-of self)))))

(def call-in-component-environment node-component ()
  (bind ((*tree-level* (1+ *tree-level*)))
    (call-next-method)))

;;;;;;
;;; Entire node

(def component entire-node-component (remote-identity-component-mixin content-component)
  ())

(def function render-entire-node (node tree body-thunk)
  (with-slots (id) node
    (list <tr (:id ,id)
              <td (:colspan ,(length (columns-of tree)))
                  ,(funcall body-thunk)>>)))

(def render entire-node-component ()
  (render-entire-node -self- (find-ancestor-component-with-type -self- 'tree-component) #'call-next-method))
