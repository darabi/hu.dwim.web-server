;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tree-level/widget

(def (component e) tree-level/widget (widget/basic collapsible/abstract)
  ((path nil :type component)
   (previous-sibling nil :type component)
   (next-sibling nil :type component)
   (descendants nil :type component)
   (node nil :type component)))

;; TODO: move this non widgetness to the viewer/inspector etc.
(def refresh-component tree-level/widget
  #+nil ;; TODO: kill find-tree/parent
  (bind (((:slots path previous-sibling next-sibling descendants node) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (parent-value (find-tree/parent -self- dispatch-class dispatch-prototype component-value))
         (path-value (nreverse (iter (for parent
                                          :initially parent-value
                                          :then (find-tree/parent -self- dispatch-class dispatch-prototype parent))
                                     (while parent)
                                     (collect parent))))
         (siblings-value (when parent-value
                           (collect-tree/children -self- dispatch-class dispatch-prototype parent-value)))
         (position (position component-value siblings-value))
         (previous-sibling-value (when (and position
                                            (< 0 position))
                                   (elt siblings-value (1- position))))
         (next-sibling-value (when (and position
                                        (< position (1- (length siblings-value))))
                               (elt siblings-value (1+ position)))))
    (if path
        (setf (component-value-of path) path-value)
        (setf path (make-tree-level/path -self- dispatch-class dispatch-prototype path-value)))
    (if previous-sibling-value
        (if previous-sibling
            (setf (component-value-of previous-sibling) previous-sibling-value)
            (setf previous-sibling (make-tree-level/previous-sibling -self- dispatch-class dispatch-prototype previous-sibling-value)))
        (setf previous-sibling nil))
    (if next-sibling-value
        (if next-sibling
            (setf (component-value-of next-sibling) next-sibling-value)
            (setf next-sibling (make-tree-level/next-sibling -self- dispatch-class dispatch-prototype next-sibling-value)))
        (setf next-sibling nil))
    (if descendants
        (setf (component-value-of descendants) component-value)
        (setf descendants (make-tree-level/descendants -self- dispatch-class dispatch-prototype component-value)))
    (if node
        (setf (component-value-of node) component-value)
        (setf node (make-tree-level/node -self- dispatch-class dispatch-prototype component-value)))))

(def render-xhtml tree-level/widget
  (bind (((:read-only-slots path previous-sibling next-sibling descendants node) -self-))
    <div (:class "tree-level")
         <span (:class "header")
              ,(render-collapse-or-expand-command-for -self-)
              ,(when (expanded-component? -self-)
                     <span (:class "previous-sibling")
                           ,(when previous-sibling
                              (render-component previous-sibling)
                              (render-component " << "))>
                     <span (:class "path")
                           ,(render-component path)>
                     <span (:class "next-sibling")
                           ,(when next-sibling
                              (render-component " >> ")
                              (render-component next-sibling))>
                     <hr>)>
         ,(when (expanded-component? -self-)
            (render-component descendants)
            <hr>)
         ,(render-component node)>))

(def (layered-function e) make-tree-level/path (component class prototype value))

(def (layered-function e) make-tree-level/previous-sibling (component class prototype value))

(def (layered-function e) make-tree-level/next-sibling (component class prototype value))

(def (layered-function e) make-tree-level/descendants (component class prototype value))

(def (layered-function e) make-tree-level/node (component class prototype value))
