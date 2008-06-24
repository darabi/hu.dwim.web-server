;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tree

(def special-variable *tree-level*)

(def component tree-component ()
  ((columns nil :type components)
   (root-node nil :type component)))

(def render tree-component ()
  (with-slots (columns root-node) -self-
    (bind ((*tree-level* 0))
      <table (:class "tree")
          <thead
           <tr <th>
               ,@(mapcar #'render columns)>>
        <tbody ,@(render root-node)>>)))

;;;;;;
;;; Node

(def component node-component ()
  ((child-nodes nil :type components)
   (cells nil :type components)
   (expanded #t :type boolean)))

(def render node-component ()
  (with-slots (child-nodes cells expanded) -self-
    (append (list <tr (:class ,(concatenate-string "level-" (integer-to-string *tree-level*)))
                      <td (:class ,(if child-nodes
                                       "expander"
                                       "non-expandable"))
                          ,(if child-nodes
                               <a (:href ,(make-action-href () (setf expanded (not expanded))))
                                  <img (:src ,(concatenate-string (path-prefix-of *application*)
                                                                  (if expanded
                                                                      "static/wui/icons/20x20/arrowhead-down.png"
                                                                      "static/wui/icons/20x20/arrowhead-right.png")))>>
                               +void+)>
                      ,@(mapcar #'render cells)>)
            (when expanded
              (bind ((*tree-level* (1+ *tree-level*)))
                (mappend #'render child-nodes))))))

;;;;;;
;;; Entire node

(def component entire-node-component ()
  ())

(def function render-entire-node (tree body-thunk)
  <tr
   <td (:colspan ,(length (columns-of tree)))
       ,(funcall body-thunk)>>)

(def render entire-node-component ()
  (render-entire-node (parent-component-of -self-) #'call-next-method))
