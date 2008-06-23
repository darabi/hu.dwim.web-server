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

(def component standard-object-tree-component (abstract-standard-object-tree-component alternator-component editable-component user-message-collector-component-mixin)
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

(def component standard-object-tree-table-component (abstract-standard-object-tree-component tree-component editable-component)
  ())

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
