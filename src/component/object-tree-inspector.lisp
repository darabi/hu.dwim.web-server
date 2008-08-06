;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object tree

(def component standard-object-tree-inspector (abstract-standard-object-tree-component
                                               abstract-standard-class-component
                                               inspector-component
                                               alternator-component
                                               editable-component
                                               user-message-collector-component-mixin
                                               remote-identity-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-tree-inspector-alternatives)
  (:documentation "Component for a tree of STANDARD-OBJECTs in various alternative views"))

(def (macro e) standard-object-tree-inspector (root children-provider &rest args)
  `(make-instance 'standard-object-tree-inspector :instance ,root :children-provider ,children-provider ,@args))

(def method refresh-component ((self standard-object-tree-inspector))
  (with-slots (instance children-provider default-component-type alternatives alternatives-factory content command-bar) self
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) instance)
              (setf alternatives (funcall alternatives-factory self (class-of instance) instance)))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) instance)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives)))))
        (setf alternatives (list (delay-alternative-component-with-initargs 'null-component))
              content (find-default-alternative-component alternatives)))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (list (make-open-in-new-frame-command self)
                                                         (make-top-command self)
                                                         (make-refresh-command self))))))

(def render standard-object-tree-inspector ()
  <div (:class "standard-object-tree")
    ,(render-user-messages -self-)
    ,(call-next-method)>)

(def (generic e) make-standard-object-tree-inspector-alternatives (component class instance)
  (:method ((component standard-object-tree-inspector) (class standard-class) (instance standard-object))
    (list (delay-alternative-component-with-initargs 'standard-object-tree-table-inspector :instance instance :children-provider (children-provider-of component))
          (delay-alternative-component-with-initargs 'standard-object-tree-nested-box-inspector :instance instance :children-provider (children-provider-of component))
          (delay-alternative-reference-component 'standard-object-tree-reference-component instance))))

;;;;;;
;;; Standard object tree table

(def component standard-object-tree-table-inspector (abstract-standard-object-tree-component
                                                     abstract-standard-class-component
                                                     inspector-component
                                                     tree-component
                                                     editable-component)
  ())

;; TODO: factor out common parts with standard-object-table-inspector
(def method refresh-component ((self standard-object-tree-table-inspector))
  (with-slots (instance root-node columns) self
    (setf columns (make-standard-object-tree-table-inspector-columns self))
    (if root-node
        (setf (component-value-of root-node) instance)
        (setf root-node (make-standard-object-tree-node self instance)))))

(def (function e) make-standard-object-tree-table-type-column ()
  (make-instance 'column-component
                 :content (label #"Object-tree-table.column.type")
                 :cell-factory (lambda (node-component)
                                 (make-instance 'cell-component :content (make-instance 'standard-class-component
                                                                                        :the-class (class-of (instance-of node-component))
                                                                                        :default-component-type 'reference-component)))))

(def (function e) make-standard-object-tree-table-command-bar-column ()
  (make-instance 'column-component
                    :content (label #"Object-tree-table.column.commands")
                    :visible (delay (not (layer-active-p 'passive-components-layer)))
                    :cell-factory (lambda (node-component)
                                    (make-instance 'cell-component :content (command-bar-of node-component)))))

(def (generic e) make-standard-object-tree-table-inspector-columns (component)
  (:method ((self standard-object-tree-table-inspector))
    (append (optional-list
             (make-standard-object-tree-table-command-bar-column)
             (when-bind the-class (the-class-of self)
               (when (closer-mop:class-direct-subclasses the-class)
                 (make-standard-object-tree-table-type-column))))
            (make-standard-object-tree-table-inspector-slot-columns self))))

(def (generic e) make-standard-object-tree-table-inspector-slot-columns (component)
  (:method ((self standard-object-tree-table-inspector))
    (with-slots (instance children-provider the-class) self
      (bind ((slot-name->slot (list)))
        ;; KLUDGE: TODO: this register mapping is wrong, it maps slot-names to randomly choosen effective-slots
        (labels ((register-slot (slot)
                   (bind ((slot-name (slot-definition-name slot)))
                     (unless (member slot-name slot-name->slot :test #'eq :key #'car)
                       (push (cons slot-name slot) slot-name->slot))))
                 (register-instance (node)
                   (dolist (child (funcall children-provider node))
                     (mapc #'register-slot (collect-standard-object-tree-table-inspector-slots self (class-of child) child))
                     (register-instance child))))
          (when the-class
            (mapc #'register-slot (collect-standard-object-tree-table-inspector-slots self the-class (class-prototype the-class))))
          (register-instance instance))
        (mapcar (lambda (slot-name->slot)
                  (make-instance 'column-component
                                 :content (label (localized-slot-name (cdr slot-name->slot)))
                                 :cell-factory (lambda (node-component)
                                                 (bind ((slot (find-slot (class-of (instance-of node-component)) (car slot-name->slot))))
                                                   (if slot
                                                       (make-instance 'standard-object-slot-value-cell-component :instance (instance-of node-component) :slot slot)
                                                       (make-instance 'cell-component :content (label "N/A")))))))
                slot-name->slot)))))

(def (generic e) collect-standard-object-tree-table-inspector-slots (component class instance)
  (:method ((component standard-object-tree-table-inspector) (class standard-class) instance)
    (class-slots class))

  (:method ((component standard-object-tree-table-inspector) (class prc::persistent-class) instance)
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-tree-table-inspector) (class dmm::entity) instance)
    (filter-if (lambda (slot)
                 (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
               (call-next-method))))

(defresources hu
  (object-tree-table.column.commands "műveletek")
  (object-tree-table.column.type "típus"))

(defresources en
  (object-tree-table.column.commands "commands")
  (object-tree-table.column.type "type"))

;;;;;;
;;; Standard object node

(def component standard-object-tree-node-inspector (abstract-standard-object-tree-component
                                                    abstract-standard-class-component
                                                    inspector-component
                                                    node-component
                                                    editable-component
                                                    user-message-collector-component-mixin)
  ((command-bar nil :type component)))

(def method refresh-component ((self standard-object-tree-node-inspector))
  (with-slots (children-provider instance command-bar child-nodes cells) self
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-object-tree-node-inspector-commands self (class-of instance) instance))
              child-nodes (iter (for child :in (funcall (children-provider-of self) instance))
                                (for node = (find instance child-nodes :key #'component-value-of))
                                (if node
                                    (setf (component-value-of node) child)
                                    (setf node (make-standard-object-tree-node (find-ancestor-component-with-type self 'tree-component) child)))
                                (collect node))
              cells (mapcar (lambda (column)
                              (funcall (cell-factory-of column) self))
                            (columns-of (find-ancestor-component-with-type self 'standard-object-tree-table-inspector))))
        (setf command-bar nil
              cells nil))))

(def (generic e) make-standard-object-tree-node (component instance)
  (:method ((component standard-object-tree-table-inspector) (instance standard-object))
    (make-instance 'standard-object-tree-node-inspector
                   :instance instance
                   :children-provider (children-provider-of component)
                   :expanded (expand-nodes-by-default-p component))))

(def render standard-object-tree-node-inspector ()
  (when (messages-of -self-)
    (render-entire-node -self- (find-ancestor-component-with-type -self- 'tree-component)
                        (lambda ()
                          (render-user-messages -self-))))
  (call-next-method))

(def generic make-standard-object-tree-node-inspector-commands (component class instance)
  (:method ((component standard-object-tree-node-inspector) (class standard-class) (instance standard-object))
    (append (make-editing-commands component)
            (list (make-expand-node-command component instance))))

  (:method ((component standard-object-tree-node-inspector) (class prc::persistent-class) (instance prc::persistent-object))
    (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
              (make-editing-commands component))
            (list (make-expand-node-command component instance)))))

(def function make-expand-node-command (component instance)
  (make-replace-and-push-back-command component (delay (make-instance '(editable-component entire-node-component) :content (make-viewer-component instance :default-component-type 'detail-component)))
                                      (list :icon (icon expand)
                                            :visible (delay (not (has-edited-descendant-component-p component))))
                                      (list :icon (icon collapse))))

;;;;;;
;;; Standard object tree nested box

(def special-variable *standard-object-tree-level* 0)

(def component standard-object-tree-nested-box-inspector (abstract-standard-object-tree-component
                                                          abstract-standard-class-component
                                                          inspector-component)
  ())

(def (macro e) standard-object-tree-nested-box-inspector (root children-provider &rest args)
  `(make-instance 'standard-object-tree-nested-box-inspector :instance ,root :children-provider ,children-provider ,@args))

(def render standard-object-tree-nested-box-inspector ()
  (labels ((render-node (node)
             (bind ((*standard-object-tree-level* (1+ *standard-object-tree-level*))
                    (children (funcall (children-provider-of -self-) node)))
               (if children
                   (if (oddp *standard-object-tree-level*)
                       <table (:class "node")
                         <thead <tr <td ,(render-standard-object-tree-nested-box-inspector-node -self- node) >>>
                         ,@(mapcar (lambda (child)
                                     <tr <td ,(render-node child)>>)
                                   children)>
                       <table (:class "node")
                         <thead <tr <td (:colspan ,(length children))
                                        ,(render-standard-object-tree-nested-box-inspector-node -self- node)>>>
                         <tr ,@(mapcar (lambda (child)
                                         <td ,(render-node child)>) children)>>)
                   <table (:class "leaf")
                     <tr <td ,(render-standard-object-tree-nested-box-inspector-node -self- node)>>>))))
    <div (:class "standard-object-tree-nested-box")
         ,(render-node (instance-of -self-))>))

(def (generic e) render-standard-object-tree-nested-box-inspector-node (component node)
  (:method ((component standard-object-tree-nested-box-inspector) (instance standard-object))
    <div ,(localized-instance-name instance)>))
