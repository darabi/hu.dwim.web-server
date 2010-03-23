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
        (setf root-nodes (mapcar [make-tree/root-node -self- dispatch-class dispatch-prototype !1] root-node-values)))))

(def (layered-function e) make-tree/root-node (component class prototype value)
  (:method ((component t/tree/inspector) class prototype value)
    (make-instance 't/node/inspector
                   :component-value value
                   :edited (edited-component? component)
                   :editable (editable-component? component))))

;;;;;;
;;; t/node/inspector

(def (component e) t/node/inspector (inspector/basic t/detail/inspector node/widget)
  ())

(def refresh-component t/node/inspector
  (bind (((:slots child-nodes content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-))
         (children (collect-tree/children -self- dispatch-class dispatch-prototype component-value)))
    (if content
        (setf (component-value-of content) component-value)
        (setf content (make-node/content -self- dispatch-class dispatch-prototype component-value)))
    (setf child-nodes (iter (for child :in-sequence children)
                            (for child-node = (find child child-nodes :key #'component-value-of))
                            (if child-node
                                (setf (component-value-of child-node) child)
                                (setf child-node (make-node/child-node -self- dispatch-class dispatch-prototype child)))
                            (collect child-node)))))

(def (layered-function e) make-node/content (component class prototype value))

(def (layered-function e) make-node/child-node (component class prototype value)
  (:method ((component t/node/inspector) class prototype value)
    (make-instance 't/node/inspector
                   :component-value value
                   :edited (edited-component? component)
                   :editable (editable-component? component))))

(def (layered-function e) collect-tree/children (component class prototype value))

(def layered-method collect-tree/children ((component t/node/inspector) (class built-in-class) (prototype list) (value list))
  value)

(def layered-method collect-tree/children ((component t/node/inspector) class prototype value)
  nil)

(def layered-method make-node/content ((component t/node/inspector) class prototype value)
  (make-value-inspector value
                        :initial-alternative-type 't/reference/presentation
                        :edited (edited-component? component)
                        :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/inspector

(def (component e) t/tree-level/inspector (inspector/basic t/detail/inspector tree-level/widget)
  ())

(def layered-method make-tree-level/path ((component t/tree-level/inspector) class prototype value)
  (make-instance 't/tree-level/path/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/reference/inspector

(def (component e) t/tree-level/reference/inspector (t/reference/inspector)
  ())

(def refresh-component t/tree-level/reference/inspector
  (bind (((:slots action component-value) -self-))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 't/tree-level/inspector)) component-value)))))

;;;;;;
;;; t/tree-level/path/inspector

(def (component e) t/tree-level/path/inspector (inspector/basic t/detail/inspector path/widget)
  ())

(def method component-dispatch-class ((self t/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-path/content ((component t/tree-level/path/inspector) class prototype value)
  (make-instance 't/tree-level/reference/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/tree/inspector

(def (component e) t/tree-level/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component t/tree-level/tree/inspector) class prototype value)
  (make-instance 't/tree-level/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; t/tree-level/node/inspector

(def (component e) t/tree-level/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component t/tree-level/node/inspector) class prototype value)
  (make-instance 't/tree-level/node/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

(def layered-method make-node/content ((component t/tree-level/node/inspector) class prototype value)
  (make-instance 't/tree-level/reference/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

;;;;;;
;;; sequence/tree/inspector

(def (component e) sequence/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component sequence/tree/inspector) class prototype value)
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

(def layered-method make-node/child-node ((component sequence/node/inspector) class prototype value)
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
    (setf columns (make-table-columns -self- class prototype component-value)
          root-nodes (iter (for node-value :in-sequence component-value)
                           (for root-node = (find node-value root-nodes :key #'component-value-of))
                           (if root-node
                               (setf (component-value-of root-node) node-value)
                               (setf root-node (make-table-row -self- class prototype node-value)))
                           (collect root-node)))))
