;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/tree/inspector

(def (component e) t/tree/inspector (inspector/basic t/detail/inspector tree/widget)
  ())

(def refresh-component t/tree/inspector
  (bind (((:slots root-nodes) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (root-node-values (ensure-sequence (component-value-of -self-))))
    (if root-nodes
        (foreach [setf (component-value-of !1) !2] root-nodes root-node-values)
        (setf root-nodes (mapcar [make-node-presentation -self- dispatch-class dispatch-prototype !1] root-node-values)))))

(def (layered-function e) make-node-presentation (component class prototype value))

(def layered-method make-node-presentation ((component t/tree/inspector) class prototype value)
  (make-instance 't/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/node/inspector

(def (component e) t/node/inspector (inspector/basic t/detail/inspector node/widget)
  ())

(def refresh-component t/node/inspector
  (bind (((:slots child-nodes content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-presented-children -self- dispatch-class dispatch-prototype component-value)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-content-presentation -self- dispatch-class dispatch-prototype component-value)))
    (setf child-nodes (iter (for child :in-sequence children)
                            (for child-node = (find child child-nodes :key #'component-value-of))
                            (if child-node
                                (setf (component-value-of child-node) child)
                                (setf child-node (make-node-presentation -self- dispatch-class dispatch-prototype child)))
                            (collect child-node)))))

(def layered-method make-node-presentation ((component t/node/inspector) class prototype value)
  (make-instance 't/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

(def (layered-function e) collect-presented-children (component class prototype value))

(def layered-method collect-presented-children ((component t/node/inspector) (class built-in-class) (prototype list) (value list))
  value)

(def layered-method collect-presented-children ((component t/node/inspector) class prototype value)
  nil)

(def layered-method make-content-presentation ((component t/node/inspector) class prototype value)
  (make-value-inspector value
                        :initial-alternative-type 't/reference/presentation
                        :edited (edited-component? component)
                        :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/inspector

(def (component e) t/tree-level/inspector (inspector/basic t/detail/inspector tree-level/widget)
  ())

(def refresh-component t/tree-level/inspector
  (bind (((:slots path previous-sibling next-sibling descendants node) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (parent-value (component-value-of (parent-component-of -self-)))
         (path-value (nreverse (iter (for parent
                                          :initially parent-value
                                          :then (component-value-of (parent-component-of -self-)))
                                     (while parent)
                                     (collect parent))))
         (siblings-value (when parent-value
                           (collect-presented-children -self- dispatch-class dispatch-prototype parent-value)))
         (position (position component-value siblings-value))
         (previous-sibling-value (when (and position
                                            (< 0 position))
                                   (elt siblings-value (1- position))))
         (next-sibling-value (when (and position
                                        (< position (1- (length siblings-value))))
                               (elt siblings-value (1+ position)))))
    (if path
        (setf (component-value-of path) path-value)
        (setf path (make-path-presentation -self- dispatch-class dispatch-prototype path-value)))
    (if previous-sibling-value
        (if previous-sibling
            (setf (component-value-of previous-sibling) previous-sibling-value)
            (setf previous-sibling (make-previous-sibling-presentation -self- dispatch-class dispatch-prototype previous-sibling-value)))
        (setf previous-sibling nil))
    (if next-sibling-value
        (if next-sibling
            (setf (component-value-of next-sibling) next-sibling-value)
            (setf next-sibling (make-next-sibling-presentation -self- dispatch-class dispatch-prototype next-sibling-value)))
        (setf next-sibling nil))
    (if descendants
        (setf (component-value-of descendants) component-value)
        (setf descendants (make-descendants-presentation -self- dispatch-class dispatch-prototype component-value)))
    (if node
        (setf (component-value-of node) component-value)
        (setf node (make-node-presentation -self- dispatch-class dispatch-prototype component-value)))))

(def (layered-function e) make-path-presentation (component class prototype value)
  (:method ((component t/tree-level/inspector) class prototype value)
    (make-instance 't/tree-level/path/inspector
                   :component-value value
                   :edited (edited-component? component)
                   :editable (editable-component? component))))

(def (layered-function e) make-previous-sibling-presentation (component class prototype value))

(def (layered-function e) make-next-sibling-presentation (component class prototype value))

(def (layered-function e) make-descendants-presentation (component class prototype value))

;;;;;;
;;; t/tree-level/reference/inspector

(def (component e) t/tree-level/reference/inspector (t/reference/inspector)
  ())

(def refresh-component t/tree-level/reference/inspector
  (bind (((:slots action component-value) -self-))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-of-type 't/tree-level/inspector -self-)) component-value)))))

;;;;;;
;;; t/tree-level/path/inspector

(def (component e) t/tree-level/path/inspector (inspector/basic t/detail/inspector path/widget)
  ())

(def method component-dispatch-class ((self t/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-content-presentation ((component t/tree-level/path/inspector) class prototype value)
  (make-instance 't/tree-level/reference/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/tree/inspector

(def (component e) t/tree-level/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-node-presentation ((component t/tree-level/tree/inspector) class prototype value)
  (make-instance 't/tree-level/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/node/inspector

(def (component e) t/tree-level/node/inspector (t/node/inspector)
  ())

(def layered-method make-node-presentation ((component t/tree-level/node/inspector) class prototype value)
  (make-instance 't/tree-level/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

(def layered-method make-content-presentation ((component t/tree-level/node/inspector) class prototype value)
  (make-instance 't/tree-level/reference/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; sequence/tree/inspector

(def (component e) sequence/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-node-presentation ((component sequence/tree/inspector) class prototype value)
  (make-instance (if (typep value 'sequence)
                     'sequence/node/inspector
                     't/node/inspector)
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; sequence/node/inspector

(def (component e) sequence/node/inspector (t/node/inspector)
  ())

(def layered-method make-node-presentation ((component sequence/node/inspector) class prototype value)
  (make-instance (if (typep value 'sequence)
                     'sequence/node/inspector
                     't/node/inspector)
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; sequence/treeble/inspector

(def (component e) sequence/treeble/inspector (inspector/basic t/detail/inspector treeble/widget)
  ())

(def refresh-component sequence/treeble/inspector
  (bind (((:slots component-value root-nodes columns) -self-)
         (class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-)))
    (setf columns (make-column-presentations -self- class prototype component-value)
          root-nodes (iter (for node-value :in-sequence component-value)
                           (for root-node = (find node-value root-nodes :key #'component-value-of))
                           (if root-node
                               (setf (component-value-of root-node) node-value)
                               (setf root-node (make-nodrow-presentation -self- class prototype node-value)))
                           (collect root-node)))))

(def (layered-function e) make-nodrow-presentation (component class prototype value)
  (:method ((component sequence/treeble/inspector) class prototype value)
    (make-instance 't/nodrow/inspector
                   :component-value value
                   :edited (edited-component? component)
                   :editable (editable-component? component))))

;;;;;;
;;; t/nodrow/inspector

(def (component e) t/nodrow/inspector (inspector/style t/detail/inspector nodrow/widget)
  ())

(def refresh-component t/nodrow/inspector
  (bind (((:slots child-nodes cells component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-presented-children -self- dispatch-class dispatch-prototype component-value)))
    (setf cells
          (if component-value
              (mapcar (lambda (column)
                        (funcall (cell-factory-of column) -self-))
                      (columns-of *tree*))
              nil))
    (setf child-nodes (iter (for child :in-sequence children)
                            (for child-node = (find child child-nodes :key #'component-value-of))
                            (if child-node
                                (setf (component-value-of child-node) child)
                                (setf child-node (make-nodrow-presentation -self- dispatch-class dispatch-prototype child)))
                            (collect child-node)))))

(def layered-method make-nodrow-presentation ((component t/nodrow/inspector) class prototype value)
  (make-instance 't/nodrow/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))
