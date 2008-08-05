;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard object tree

(def component abstract-standard-object-tree-component (abstract-standard-object-component)
  ((children-provider nil :type (or symbol function)))
  (:documentation "Base class for a tree of STANDARD-OBJECT component value"))

(def method clone-component ((self abstract-standard-object-tree-component))
  (prog1-bind clone (call-next-method)
    (setf (children-provider-of clone) (children-provider-of self))))

(def special-variable *standard-object-tree-level* 0)

;;;;;;
;;; Standard object tree

(def component standard-object-tree-component (abstract-standard-object-tree-component alternator-component editable-component user-message-collector-component-mixin)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-tree-alternatives)
  (:documentation "Component for a tree of STANDARD-OBJECTs in various alternative views"))

(def (macro e) standard-object-tree-component (root children-provider &rest args)
  `(make-instance 'standard-object-tree-component :instance ,root :children-provider ,children-provider ,@args))

(def method refresh-component ((self standard-object-tree-component))
  (with-slots (instance children-provider default-component-type alternatives alternatives-factory content command-bar) self
    (if instance
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) instance)
              (setf alternatives (funcall alternatives-factory self instance)))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) instance)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives)))))
        (setf alternatives (list (delay-alternative-component-type 'null-component))
              content (find-default-alternative-component alternatives)))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (list (make-open-in-new-frame-command self)
                                                         (make-top-command self)
                                                         (make-refresh-command self))))))

(def render standard-object-tree-component ()
  <div (:class "standard-object-tree")
    ,(render-user-messages -self-)
    ,(call-next-method)>)

(def (generic e) make-standard-object-tree-alternatives (component instance)
  (:method ((component standard-object-tree-component) instance)
    (list (delay-alternative-component-type 'standard-object-tree-table-component :instance instance :children-provider (children-provider-of component))
          (delay-alternative-component-type 'standard-object-tree-nested-box-component :instance instance :children-provider (children-provider-of component))
          (delay-alternative-component 'standard-object-tree-reference-component
            (setf-expand-reference-to-default-alternative-command
             (make-instance 'standard-object-tree-reference-component :target instance))))))

;;;;;;
;;; Standard object tree table

(def component standard-object-tree-table-component (abstract-standard-object-tree-component tree-component editable-component)
  ((slot-names nil)))

;; TODO: factor out common parts with standard-object-table-component
(def method refresh-component ((self standard-object-tree-table-component))
  (with-slots (instance root-node slot-names columns) self
    (bind ((slot-name->slot (list)))
      (setf slot-names (mapcar #'car slot-name->slot))
      (setf columns (make-standard-object-tree-table-columns self))
      (if root-node
          (setf (component-value-of root-node) instance)
          (setf root-node (make-standard-object-tree-node self instance))))))

(def (function e) make-standard-object-tree-table-type-column ()
  (make-instance 'column-component
                 :content (label #"Object-tree-table.column.type")
                 :cell-factory (lambda (node-component)
                                 (make-instance 'cell-component :content (make-instance 'standard-class-component
                                                                                        :the-class (the-class-of node-component)
                                                                                        :default-component-type 'reference-component)))))

(def (function e) make-standard-object-tree-table-command-bar-column ()
  (make-instance 'column-component
                    :content (label #"Object-tree-table.column.commands")
                    :visible (delay (not (layer-active-p 'passive-components-layer)))
                    :cell-factory (lambda (node-component)
                                    (make-instance 'cell-component :content (command-bar-of node-component)))))

(def (generic e) make-standard-object-tree-table-columns (component)
  (:method ((self standard-object-tree-table-component))
    (list*
     (make-standard-object-tree-table-command-bar-column)
     (make-standard-object-tree-table-type-column)
     (make-standard-object-tree-table-slot-columns self))))

(def (generic e) make-standard-object-tree-table-slot-columns (component)
  (:method ((self standard-object-tree-table-component))
    (with-slots (instance children-provider the-class) self
      (bind ((slot-name->slot (list)))
        ;; KLUDGE: TODO: this register mapping is wrong, it maps slot-names to randomly choosen effective-slots
        (labels ((register-slot (slot)
                   (bind ((slot-name (slot-definition-name slot)))
                     (unless (member slot-name slot-name->slot :test #'eq :key #'car)
                       (push (cons slot-name slot) slot-name->slot))))
                 (register-instance (node)
                   (dolist (child (funcall children-provider node))
                     (mapc #'register-slot (collect-standard-object-tree-table-slots self (class-of child) child))
                     (register-instance child))))
          (when the-class
            (mapc #'register-slot (collect-standard-object-tree-table-slots self the-class (class-prototype the-class))))
          (register-instance instance))
        (mapcar (lambda (slot-name->slot)
                  (make-instance 'column-component
                                 :content (label (localized-slot-name (cdr slot-name->slot)))
                                 :cell-factory (lambda (node-component)
                                                 (bind ((slot (find-slot (the-class-of node-component) (car slot-name->slot))))
                                                   (if slot
                                                       (make-instance 'standard-object-slot-value-cell-component :instance (instance-of node-component) :slot slot)
                                                       (make-instance 'cell-component :content (label "N/A")))))))
                slot-name->slot)))))

(def (generic e) collect-standard-object-tree-table-slots (component class instance)
  (:method ((component standard-object-tree-table-component) (class standard-class) instance)
    (class-slots class))

  (:method ((component standard-object-tree-table-component) (class prc::persistent-class) instance)
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((component standard-object-tree-table-component) (class dmm::entity) instance)
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

(def component standard-object-tree-node-component (abstract-standard-object-tree-component node-component editable-component user-message-collector-component-mixin)
  ((command-bar nil :type component)))

(def method refresh-component ((self standard-object-tree-node-component))
  (with-slots (children-provider the-class instance command-bar child-nodes cells) self
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-object-tree-node-commands self the-class instance))
              child-nodes (iter (for child :in (funcall (children-provider-of self) instance))
                                (for node = (find instance child-nodes :key #'component-value-of))
                                (if node
                                    (setf (component-value-of node) child)
                                    (setf node (make-standard-object-tree-node (find-ancestor-component-with-type self 'tree-component) child)))
                                (collect node))
              cells (mapcar (lambda (column)
                              (funcall (cell-factory-of column) self))
                            (columns-of (find-ancestor-component-with-type self 'standard-object-tree-table-component))))
        (setf command-bar nil
              cells nil))))

(def (generic e) make-standard-object-tree-node (component instance)
  (:method ((component standard-object-tree-table-component) (instance standard-object))
    (make-instance 'standard-object-tree-node-component
                   :instance instance
                   :children-provider (children-provider-of component)
                   :expanded (expand-nodes-by-default-p component))))

(def render standard-object-tree-node-component ()
  (when (messages-of -self-)
    (render-entire-node -self- (find-ancestor-component-with-type -self- 'tree-component)
                        (lambda ()
                          (render-user-messages -self-))))
  (call-next-method))

(def generic make-standard-object-tree-node-commands (component class instance)
  (:method ((component standard-object-tree-node-component) (class standard-class) (instance standard-object))
    (append (make-editing-commands component)
            (list (make-expand-node-command component instance))))

  (:method ((component standard-object-tree-node-component) (class prc::persistent-class) (instance prc::persistent-object))
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

(def component standard-object-tree-nested-box-component (abstract-standard-object-tree-component)
  ())

(def (macro e) standard-object-tree-nested-box-component (root children-provider &rest args)
  `(make-instance 'standard-object-tree-nested-box-component :instance ,root :children-provider ,children-provider ,@args))

(def render standard-object-tree-nested-box-component ()
  (labels ((render-node (node)
             (bind ((*standard-object-tree-level* (1+ *standard-object-tree-level*))
                    (children (funcall (children-provider-of -self-) node)))
               (if children
                   (if (oddp *standard-object-tree-level*)
                       <table (:class "node")
                         <thead <tr <td ,(render-standard-object-tree-nested-box-node -self- node) >>>
                         ,@(mapcar (lambda (child)
                                     <tr <td ,(render-node child)>>)
                                   children)>
                       <table (:class "node")
                         <thead <tr <td (:colspan ,(length children))
                                        ,(render-standard-object-tree-nested-box-node -self- node)>>>
                         <tr ,@(mapcar (lambda (child)
                                         <td ,(render-node child)>) children)>>)
                   <table (:class "leaf")
                     <tr <td ,(render-standard-object-tree-nested-box-node -self- node)>>>))))
    <div (:class "standard-object-tree-nested-box")
         ,(render-node (instance-of -self-))>))

(def (generic e) render-standard-object-tree-nested-box-node (component node))
