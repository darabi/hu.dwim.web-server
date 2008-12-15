;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object tree inspector

(def component standard-object-tree-inspector (abstract-standard-object-tree-component
                                               inspector-component
                                               alternator-component
                                               editable-component
                                               exportable-component
                                               user-message-collector-component-mixin
                                               remote-identity-component-mixin
                                               initargs-component-mixin
                                               recursion-point-component)
  ()
  (:default-initargs :alternatives-factory #'make-standard-object-tree-inspector-alternatives :the-class (find-class 'standard-object))
  (:documentation "Component for a tree of STANDARD-OBJECTs in various alternative views"))

(def (macro e) standard-object-tree-inspector (root children-provider parent-provider &rest args)
  `(make-instance 'standard-object-tree-inspector :instance ,root :children-provider ,children-provider :parent-provider ,parent-provider ,@args))

(def method refresh-component ((self standard-object-tree-inspector))
  (with-slots (instances the-class children-provider default-component-type alternatives alternatives-factory content command-bar) self
    (if alternatives
        (setf (component-value-for-alternatives self) instances)
        (setf alternatives (funcall alternatives-factory self the-class (class-prototype the-class) instances)))
    (if content
        (setf (component-value-of content) instances)
        (setf content (if default-component-type
                          (find-alternative-component alternatives default-component-type)
                          (find-default-alternative-component alternatives))))
    (setf command-bar (make-alternator-command-bar self alternatives
                                                   (make-standard-commands self the-class (class-prototype the-class))))))

(def render standard-object-tree-inspector ()
  (bind (((:read-only-slots id content) -self-))
    (flet ((body ()
             (render-user-messages -self-)
             (call-next-method)))
      (if (typep content 'reference-component)
          <span (:id ,id :class "standard-object-tree-insepctor")
            ,(body)>
          (progn
            <div (:id ,id :class "standard-object-tree-insepctor")
              ,(body)>
            `js(on-load (wui.setup-standard-object-tree-inspector ,id)))))))

(def (layered-function e) make-standard-object-tree-inspector-alternatives (component class prototype instances)
  (:method ((component standard-object-tree-inspector) (class standard-class) (prototype standard-object) (instances list))
    (list (delay-alternative-component-with-initargs 'standard-object-tree-table-inspector
                                                     :instances instances
                                                     :the-class (the-class-of component)
                                                     :children-provider (children-provider-of component)
                                                     :parent-provider (parent-provider-of component))
          (delay-alternative-component-with-initargs 'standard-object-tree-nested-box-inspector
                                                     :instances instances
                                                     :the-class (the-class-of component)
                                                     :children-provider (children-provider-of component)
                                                     :parent-provider (parent-provider-of component))
          (delay-alternative-component-with-initargs 'standard-object-tree-level-inspector
                                                     :instances instances
                                                     :the-class (the-class-of component)
                                                     :children-provider (children-provider-of component)
                                                     :parent-provider (parent-provider-of component))
          (delay-alternative-reference-component 'standard-object-tree-inspector-reference instances))))

;;;;;;
;;; Standard object tree table inspector

(def component standard-object-tree-table-inspector (abstract-standard-object-tree-component
                                                     inspector-component
                                                     tree-component
                                                     editable-component
                                                     title-component-mixin)
  ((class nil :accessor nil :type component))
  (:default-initargs :expander-column-index 1))

;; TODO: factor out common parts with standard-object-list-table-inspector
(def method refresh-component ((self standard-object-tree-table-inspector))
  (with-slots (instances the-class class root-nodes columns) self
    (if the-class
          (if class
              (when (typep the-class 'abstract-standard-class-component)
                (setf (the-class-of class) the-class))
              (setf class (make-class-presentation self the-class (class-prototype the-class))))
          (setf class nil))
    (setf columns (make-standard-object-tree-table-inspector-columns self))
    (if root-nodes
        (foreach [setf (component-value-of !1) !2] root-nodes instances)
        (setf root-nodes (mapcar [make-standard-object-tree-table-node self (class-of !1) !1] instances)))))

(def render standard-object-tree-table-inspector ()
  <div (:id ,(id-of -self-))
       ,(render-title -self-)
       ,(call-next-method)>)

(def layered-method render-title ((self standard-object-tree-table-inspector))
  (standard-object-tree-table-inspector.title (slot-value self 'class)))

(def (function e) make-standard-object-tree-table-type-column ()
  (make-instance 'column-component
                 :content (label #"object-tree-table.column.type")
                 :cell-factory (lambda (node-component)
                                 (bind ((class (class-of (instance-of node-component))))
                                   (make-instance 'cell-component :content (make-class-presentation node-component class (class-prototype class)))))))

(def (function e) make-standard-object-tree-table-command-bar-column ()
  (make-instance 'column-component
                 :content (label #"object-tree-table.column.commands")
                 :visible (delay (not (layer-active-p 'passive-components-layer)))
                 :cell-factory (lambda (node-component)
                                 (make-instance 'cell-component :content (command-bar-of node-component)))))

(def (layered-function e) make-standard-object-tree-table-inspector-columns (component)
  (:method ((self standard-object-tree-table-inspector))
    (append (optional-list
             (make-standard-object-tree-table-command-bar-column)
             (when-bind the-class (the-class-of self)
               (when (closer-mop:class-direct-subclasses the-class)
                 (make-standard-object-tree-table-type-column))))
            (make-standard-object-tree-table-inspector-slot-columns self))))

(def (generic e) make-standard-object-tree-table-inspector-slot-columns (component)
  (:method ((self standard-object-tree-table-inspector))
    (with-slots (instances children-provider the-class) self
      (bind ((slot-name->slot-map (list)))
        ;; KLUDGE: TODO: this register mapping is wrong, it maps slot-names to randomly choosen effective-slots
        (labels ((register-slot (slot)
                   (bind ((slot-name (slot-definition-name slot)))
                     (unless (member slot-name slot-name->slot-map :test #'eq :key #'car)
                       (push (cons slot-name slot) slot-name->slot-map))))
                 (register-instance (instance)
                   (mapc #'register-slot (collect-standard-object-tree-table-inspector-slots self (class-of instance) instance))
                   (dolist (child (funcall children-provider instance))
                     (register-instance child))))
          (when the-class
            (mapc #'register-slot (collect-standard-object-tree-table-inspector-slots self the-class (class-prototype the-class))))
          (foreach #'register-instance instances))
        (mapcar (lambda (slot-name->slot)
                  (make-standard-object-tree-table-inspector-slot-column (localized-slot-name (cdr slot-name->slot)) (car slot-name->slot)))
                (nreverse slot-name->slot-map))))))

(def (function e) make-standard-object-tree-table-inspector-slot-column (label slot-name)
  (make-instance 'column-component
                 :content label
                 :cell-factory (lambda (node-component)
                                 (bind ((slot (find-slot (class-of (instance-of node-component)) slot-name)))
                                   (if slot
                                       (make-instance 'standard-object-slot-value-cell-component :instance (instance-of node-component) :slot slot)
                                       "")))))

(def (layered-function e) collect-standard-object-tree-table-inspector-slots (component class instance)
  (:method ((component standard-object-tree-table-inspector) (class standard-class) instance)
    (class-slots class)))

(def resources hu
  (standard-object-tree-table-inspector.title (class)
    `xml,"Egy " (render class) `xml," fa megjelenítése")
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Típus"))

(def resources en
  (standard-object-tree-table-inspector.title (class)
    `xml,"Viewing a tree of " (render class))
  (object-tree-table.column.commands "")
  (object-tree-table.column.type "Type"))

;;;;;;
;;; Standard object node inspector

(def component standard-object-tree-node-inspector (abstract-standard-object-node-component
                                                    inspector-component
                                                    node-component
                                                    editable-component
                                                    user-message-collector-component-mixin)
  ((command-bar nil :type component)))

(def method refresh-component ((self standard-object-tree-node-inspector))
  (with-slots (children-provider instance command-bar child-nodes cells) self
    (if instance
        (setf command-bar (make-commands-presentation self (make-standard-commands self (class-of instance) instance))
              child-nodes (sort-child-nodes self
                                            (iter (for child :in (funcall (children-provider-of self) instance))
                                                  (for node = (find instance child-nodes :key #'component-value-of))
                                                  (if node
                                                      (setf (component-value-of node) child)
                                                      (setf node (make-standard-object-tree-table-node (find-ancestor-component-with-type self 'tree-component) (class-of child) child)))
                                                  (collect node)))
              cells (mapcar (lambda (column)
                              (funcall (cell-factory-of column) self))
                            (columns-of (find-ancestor-component-with-type self 'standard-object-tree-table-inspector))))
        (setf command-bar nil
              cells nil))))

(def (layered-function e) sort-child-nodes (parent-node child-nodes)
  (:method ((self standard-object-tree-node-inspector) (child-nodes list))
    child-nodes))

(def layered-method make-commands-presentation ((component standard-object-tree-node-inspector) (commands list))
  (make-instance 'popup-command-menu-component :commands commands))

(def (layered-function e) make-standard-object-tree-table-node (component class instance)
  (:method ((component standard-object-tree-table-inspector) (class standard-class) (instance standard-object))
    (make-instance 'standard-object-tree-node-inspector
                   :instance instance
                   :children-provider (children-provider-of component)
                   :parent-provider (parent-provider-of component)
                   :expanded (expand-nodes-by-default-p component))))

(def render standard-object-tree-node-inspector ()
  (when (messages-of -self-)
    (render-entire-node (find-ancestor-component-with-type -self- 'tree-component) -self-
                        (lambda ()
                          (render-user-messages -self-))))
  (call-next-method))

(def layered-method render-onclick-handler ((self standard-object-tree-node-inspector))
  (when-bind expand-command (find-command-bar-command (command-bar-of self) 'expand)
    (render-onclick-handler expand-command)))

(def layered-method make-standard-commands ((component standard-object-tree-node-inspector) (class standard-class) (instance standard-object))
  (append (optional-list (make-expand-command component class instance))
          (call-next-method)))

(def layered-method make-move-commands ((component standard-object-tree-node-inspector) (class standard-class) (instance standard-object))
  nil)

(def layered-method make-expand-command ((component standard-object-tree-node-inspector) (class standard-class) (instance standard-object))
  (make-replace-and-push-back-command component (delay (make-instance '(editable-component entire-node-component) :content (make-viewer instance :default-component-type 'detail-component)))
                                      (list :content (icon expand) :visible (delay (not (has-edited-descendant-component-p component))) :ajax #t)
                                      (list :content (icon collapse) :ajax #t)))

;;;;;;
;;; Standadr object tree level inspector

(def component standard-object-tree-level-inspector (abstract-standard-object-tree-component
                                                     inspector-component)
  ((current-instance nil)
   (path nil :type component)
   (children nil :type component)
   (level nil :type component)))

(def method refresh-component ((self standard-object-tree-level-inspector))
  (with-slots (current-instance path children level instance) self
    (unless current-instance
      (setf current-instance instance))
    (setf current-instance (reuse-standard-object-instance (class-of current-instance) current-instance))
    (if current-instance
        (progn
          (if path
              (setf (component-value-of path) (collect-path-to-root self))
              (setf path (make-instance 'standard-object-tree-path-inspector :instances (collect-path-to-root self))))
          (bind ((child-instances (funcall (children-provider-of self) current-instance)))
            (if children
                (setf (component-value-of children) child-instances)
                (setf children (make-standard-object-tree-children self (the-class-of self) child-instances))))
          (if level
              (setf (component-value-of level) current-instance)
              (setf level (make-standard-object-tree-level self (class-of current-instance) current-instance)))))))

(def (generic e) make-standard-object-tree-children (component class instances)
  (:method ((component standard-object-tree-level-inspector) (class standard-class) (instances list))
    (make-instance 'standard-object-list-list-inspector
                   :instances instances
                   :component-factory (lambda (list-component class instance)
                                        (declare (ignore list-component class))
                                        (make-instance 'standard-object-inspector-reference
                                                       :target instance
                                                       :expand-command (command (icon expand)
                                                                                (make-action
                                                                                  (setf (current-instance-of component) instance)
                                                                                  (mark-outdated component))))))))

(def (generic e) make-standard-object-tree-level (component class instance)
  (:method ((component standard-object-tree-level-inspector) (class standard-class) (instance standard-object))
    (make-viewer instance)))

(def function collect-path-to-root (component)
  (labels ((%collect-path-to-root (tree-instance)
             (unless (null tree-instance)
               (cons tree-instance (%collect-path-to-root (funcall (parent-provider-of component) tree-instance))))))
    (nreverse (%collect-path-to-root (current-instance-of component)))))

(def render standard-object-tree-level-inspector ()
  (bind (((:read-only-slots path children level) -self-))
    (when path
      <div ,(render path)
           ,(render children)
           ,(render level)>)))

;;;;;;
;;; Standadr object tree path inspector

(def component standard-object-tree-path-inspector (abstract-standard-object-list-component
                                                    inspector-component)
  ((segments nil :type components)))

(def method refresh-component ((self standard-object-tree-path-inspector))
  (with-slots (segments instances) self
    (setf segments (mapcar #'localized-instance-name instances))))

(def render standard-object-tree-path-inspector ()
  (bind (((:read-only-slots segments instances) -self-))
    (iter (for instance :in instances)
          (for segment :in segments)
          (rebind (instance)
            <span ,(unless (first-iteration-p) " / ")
                  <a (:href ,(action/href ()
                               (bind ((parent (parent-component-of -self-)))
                                 (setf (current-instance-of parent) instance)
                                 (setf (outdated-p parent) #t))))
                     ,segment>>))))

;;;;;;
;;; Standard object tree nested box inspector

(def special-variable *standard-object-tree-level* 0)

(def component standard-object-tree-nested-box-inspector (abstract-standard-object-tree-component
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
                         <thead <tr <th ,(render-standard-object-tree-nested-box-inspector-node -self- node) >>>
                         ,(foreach (lambda (child)
                                     <tr <td ,(render-node child)>>)
                                   children)>
                       <table (:class "node")
                         <thead <tr <th (:colspan ,(length children))
                                        ,(render-standard-object-tree-nested-box-inspector-node -self- node)>>>
                         <tr ,(foreach (lambda (child)
                                         <td ,(render-node child)>)
                                       children)>>)
                   <table (:class "leaf")
                     <tr <td ,(render-standard-object-tree-nested-box-inspector-node -self- node)>>>))))
    <div (:class "standard-object-tree-nested-box")
         ,(render-node (instance-of -self-))>))

(def (generic e) render-standard-object-tree-nested-box-inspector-node (component node)
  (:method ((component standard-object-tree-nested-box-inspector) (instance standard-object))
    <div ,(localized-instance-name instance)>))

;;;;;
;;; Selectable standard object tree inspector

(def component selectable-standard-object-tree-inspector (standard-object-tree-inspector)
  ())

(def method selected-instance-of ((self selectable-standard-object-tree-inspector))
  (awhen (content-of self)
    (selected-instance-of it)))

(def layered-method make-standard-object-tree-inspector-alternatives ((component selectable-standard-object-tree-inspector) (class standard-class) (prototype standard-object) (instances list))
  (list* (delay-alternative-component-with-initargs 'selectable-standard-object-tree-table-inspector
                                                    :instances instances
                                                    :the-class (the-class-of component)
                                                    :children-provider (children-provider-of component)
                                                    :parent-provider (parent-provider-of component))
         (call-next-method)))

;;;;;;
;;; Selectable standard object tree table inspector

(def component selectable-standard-object-tree-table-inspector (standard-object-tree-table-inspector abstract-selectable-standard-object-component)
  ())

(def layered-method make-standard-object-tree-table-node ((component selectable-standard-object-tree-table-inspector) (class standard-class) (instance standard-object))
  (make-instance 'selectable-standard-object-tree-node-inspector
                 :instance instance
                 :children-provider (children-provider-of component)
                 :expanded (expand-nodes-by-default-p component)))

(def layered-method render-title ((self selectable-standard-object-tree-table-inspector))
  (selectable-standard-object-tree-table-inspector.title (slot-value self 'class)))

(def resources en
  (selectable-standard-object-tree-table-inspector.title (class)
    `xml,"Selecting an instance of " (render class)))

(def resources hu
  (selectable-standard-object-tree-table-inspector.title (class)
    `xml,"Egy " (render class) `xml," kiválasztása"))

;;;;;;
;;; Selectable standard object tree node inspector

(def component selectable-standard-object-tree-node-inspector (standard-object-tree-node-inspector)
  ())

(def (layered-function e) selected-component-p (component)
  (:method ((self selectable-standard-object-tree-node-inspector))
    (selected-instance-p (find-ancestor-component-with-type self 'abstract-selectable-standard-object-component) (instance-of self))))

(def layered-method tree-node-style-class ((self selectable-standard-object-tree-node-inspector))
  (concatenate-string (call-next-method)
                      (when (selected-component-p self)
                        " selected")))

(def layered-method make-standard-commands ((component selectable-standard-object-tree-node-inspector) (class standard-class) (instance standard-object))
  (append (call-next-method) (optional-list (make-select-instance-command component class instance))))

(def layered-method render-onclick-handler ((self selectable-standard-object-tree-node-inspector))
  (if-bind select-command (find-command-bar-command (command-bar-of self) 'select)
    (render-onclick-handler select-command)
    (call-next-method)))
