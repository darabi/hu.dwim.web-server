;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Abstract standard object tree

(def component abstract-standard-object-tree-component (value-component)
  ((root nil :type (or null standard-object))
   (children-provider :type (or symbol function))))

(def method component-value-of ((component abstract-standard-object-tree-component))
  (root-of component))

(def method (setf component-value-of) (new-value (component abstract-standard-object-tree-component))
  (setf (root-of component) new-value))

(def special-variable *standard-object-tree-level* 0)

;;;;;;
;;; Standard object tree

(def component standard-object-tree-component (abstract-standard-object-tree-component abstract-standard-class-component
                                               alternator-component editable-component user-message-collector-component-mixin)
  ()
  (:documentation "Component for a tree of STANDARD-OBJECTs in various alternative views"))

(def (macro e) standard-object-tree-component (root children-provider &rest args)
  `(make-instance 'standard-object-tree-component :root ,root :children-provider ,children-provider ,@args))

(def method (setf component-value-of) :after (new-value (self standard-object-tree-component))
  (with-slots (root children-provider default-component-type alternatives content command-bar) self
    (if root
        (progn
          (if (and alternatives
                   (not (typep content 'null-component)))
              (setf (component-value-for-alternatives self) root)
              (setf alternatives (list (delay-alternative-component-type 'standard-object-tree-table-component :root root :children-provider children-provider)
                                       (delay-alternative-component-type 'standard-object-tree-nested-box-component :root root :children-provider children-provider)
                                       (delay-alternative-component 'standard-object-tree-reference-component
                                         (setf-expand-reference-to-default-alternative-command
                                          (make-instance 'standard-object-tree-reference-component :target root))))))
          (if (and content
                   (not (typep content 'null-component)))
              (setf (component-value-of content) root)
              (setf content (if default-component-type
                                (find-alternative-component alternatives default-component-type)
                                (find-default-alternative-component alternatives))))
          (setf command-bar (make-instance 'command-bar-component
                                           :commands (append (list (make-open-in-new-frame-command self)
                                                                   (make-top-command self)
                                                                   (make-refresh-command self))
                                                             (make-alternative-commands self alternatives)))))
        (setf alternatives (list (delay-alternative-component-type 'null-component))
              content (find-default-alternative-component alternatives)))))

(def render standard-object-tree-component ()
  <div (:class "standard-object-tree")
    ,(render-user-messages -self-)
    ,(call-next-method)>)

;;;;;;
;;; Standard object tree table

(def component standard-object-tree-table-component (abstract-standard-object-tree-component abstract-standard-class-component tree-component editable-component)
  ((slot-names nil)))

;; TODO: factor out common parts with standard-object-table-component
(def method (setf component-value-of) :after (new-value (component standard-object-tree-table-component))
  (with-slots (root root-node children-provider the-class slot-names command-bar columns) component
    (bind ((slot-name->slot (list)))
      ;; KLUDGE: TODO: this register mapping is wrong, maps slot-names to randomly choosen effective-slots, must be forbidden
      (labels ((register-slot (slot)
                 (bind ((slot-name (slot-definition-name slot)))
                   (unless (member slot-name slot-name->slot :test #'eq :key #'car)
                     (push (cons slot-name slot) slot-name->slot))))
               (register-node (node)
                 (dolist (child (funcall children-provider node))
                   (mapc #'register-slot (standard-object-tree-table-slots (class-of child) child))
                   (register-node child))))
        (when the-class
          (mapc #'register-slot (standard-object-tree-table-slots the-class nil)))
        (register-node root))
      (setf slot-names (mapcar #'car slot-name->slot))
      (setf columns (list*
                     (column (label #"Object-tree-table.column.commands") :visible (delay (not (layer-active-p 'passive-components-layer))))
                     (column (label #"Object-tree-table.column.type"))
                     (mapcar [column (label (localized-slot-name (cdr !1)))]
                             slot-name->slot)))
      (setf root-node (if root-node
                          (setf (component-value-of root-node) root)
                          (make-instance 'standard-object-tree-node-component :instance root :children-provider children-provider :table-slot-names slot-names))))))

(def generic standard-object-tree-table-slots (class instance)
  (:method ((class standard-class) instance)
    (class-slots class))

  (:method ((class prc::persistent-class) instance)
    (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

  (:method ((class dmm::entity) instance)
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

(def component standard-object-tree-node-component (abstract-standard-object-component abstract-standard-object-tree-component
                                                    node-component editable-component user-message-collector-component-mixin)
  ((table-slot-names)
   (command-bar nil :type component)))

(def method (setf component-value-of) :after (new-value (self standard-object-tree-node-component))
  (with-slots (children-provider the-class instance table-slot-names command-bar child-nodes cells) self
    (setf instance new-value)
    (if instance
        (setf command-bar (make-instance 'command-bar-component :commands (make-standard-object-tree-node-commands self the-class instance))
              child-nodes (iter (for child :in (funcall (children-provider-of self) instance))
                                (for node = (find instance child-nodes :key #'component-value-of))
                                (if node
                                    (setf (component-value-of node) child)
                                    (setf node (make-instance 'standard-object-tree-node-component :instance child :children-provider children-provider :table-slot-names table-slot-names)))
                                (collect node))
              cells (list* (make-instance 'cell-component :content command-bar)
                           (make-instance 'cell-component :content (make-instance 'standard-class-component :the-class the-class :default-component-type 'reference-component))
                           (iter (for class = (class-of instance))
                                 (for table-slot-name :in table-slot-names)
                                 (for slot = (find-slot class table-slot-name))
                                 (for cell = (find slot cells :key (lambda (cell)
                                                                     (when (typep cell 'standard-object-slot-value-cell-component)
                                                                       (component-value-of cell)))))
                                 (collect (if slot
                                              (make-instance 'standard-object-slot-value-cell-component :instance instance :slot slot)
                                              (make-instance 'cell-component :content (label "N/A")))))))
        (setf command-bar nil
              cells nil))))

(def render standard-object-tree-node-component ()
  (when (messages-of -self-)
    (render-entire-node (parent-component-of -self-)
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
  `(make-instance 'standard-object-tree-nested-box-component :root ,root :children-provider ,children-provider ,@args))

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
         ,(render-node (root-of -self-))>))

(def (generic e) render-standard-object-tree-nested-box-node (component node))
