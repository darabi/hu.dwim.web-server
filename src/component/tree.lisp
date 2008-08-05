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
  (with-slots (columns root-node id) -self-
    <table (:id ,id :class "tree")
      <thead <tr ,@(mapcar #'render columns)>>
      <tbody ,@(render root-node)>>))

(def call-in-component-environment tree-component ()
  (bind ((*tree-level* -1))
    (call-next-method)))

;;;;;;
;;; Node

(def component node-component (remote-identity-component-mixin style-component-mixin)
  ((child-nodes nil :type components)
   (cells nil :type components)))

(def render node-component ()
  (with-slots (child-nodes cells expanded id) -self-
    (append (list <tr (:id ,id
                       :style ,(style-of -self-)
                       :class ,(concatenate-string "level-" (integer-to-string *tree-level*)
                                                   " " (css-class-of -self-)))
                      <td (:class "expander")
                          ,(if child-nodes
                               <a (:href ,(make-action-href () (setf expanded (not expanded))))
                                  <img (:src ,(concatenate-string (path-prefix-of *application*)
                                                                  (if expanded
                                                                      "static/wui/icons/20x20/arrowhead-down.png"
                                                                      "static/wui/icons/20x20/arrowhead-right.png")))>>
                               <span (:class "non-expandable")>)
                          ,(bind ((first-cell (first cells)))
                             (ensure-uptodate first-cell)
                             (render (content-of first-cell))) >
                      ,@(mapcar #'render (rest cells))>)
            (when expanded
              (mappend #'render child-nodes)))))

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
