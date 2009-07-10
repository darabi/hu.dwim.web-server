;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard object tree filter

(def (component e) standard-object-tree-filter (standard-object-filter)
  ((parent-provider :type (or symbol function))
   (children-provider :type (or symbol function))))

(def layered-method make-standard-object-filter-result-inspector ((filter standard-object-tree-filter) (instances list))
  (make-instance 'filtered-standard-object-tree-inspector
                 :instance (awhen instances
                             (find-root (first instances) (parent-provider-of filter)))
                 :filtered-instances (make-hash-set-from-list instances :test #'eql :key #'hash-key-for)
                 :parent-provider (parent-provider-of filter)
                 :unfiltered-children-provider (children-provider-of filter)))

;;;;;;
;;; Filtered standard object tree inspector

(def (component e) filtered-standard-object-tree-inspector (standard-object-tree-inspector)
  ((filtered-instances :type list)
   (highlight-filtered-instances #t :type boolean)
   (unfiltered-children-provider :type (or symbol function))
   (visible-instances (make-hash-table))))

(def constructor filtered-standard-object-tree-inspector ()
  (setf (children-provider-of -self-)
        (lambda (instance)
          (filter-if (lambda (child)
                       (gethash (hash-key-for child) (visible-instances-of -self-)))
                     (funcall (unfiltered-children-provider-of -self-) instance)))))

(def layered-method refresh-component :before ((self filtered-standard-object-tree-inspector))
  (bind (((:slots filtered-instances unfiltered-children-provider visible-instances children-provider parent-provider) self))
    (flet ((collect-visible-instance (instance)
             (setf (gethash (hash-key-for instance) visible-instances) #t)))
      (iter (for (key value) :in-hashtable filtered-instances)
            (setf value (reuse-standard-object-instance (class-of value) value))
            (map-tree value unfiltered-children-provider #'collect-visible-instance)
            (map-parent-chain value parent-provider #'collect-visible-instance)))))

(def layered-method make-alternatives ((component filtered-standard-object-tree-inspector) (class standard-class) (prototype standard-object) (instances list))
  (list* (delay-alternative-component-with-initargs 'filtered-standard-object-tree-table-inspector
                                                    :instances instances
                                                    :the-class class
                                                    :children-provider (children-provider-of component)
                                                    :parent-provider (parent-provider-of component))
         (call-next-method)))

;;;;;;
;;; Filtered standard object tree table inspector

(def (component e) filtered-standard-object-tree-table-inspector (standard-object-tree-table-inspector)
  ())

#+nil ; TODO this is not always desirable by default, e.g. in felmérési-alany-tree-table-inspector
(def layered-method refresh-component :before ((self filtered-standard-object-tree-table-inspector))
  (setf (expand-nodes-by-default-p self)
        (< (hash-table-count (visible-instances-of (parent-component-of self))) 100)))

(def layered-method make-standard-object-tree-table-node :around ((component filtered-standard-object-tree-table-inspector) (class standard-class) (instance standard-object))
  (prog1-bind tree-node
      (call-next-method)
    (bind ((filtered-standard-object-tree-inspector (parent-component-of component)))
      (when (and (highlight-filtered-instances-p filtered-standard-object-tree-inspector)
                 (gethash (hash-key-for instance) (filtered-instances-of filtered-standard-object-tree-inspector)))
        (setf (style-class-of tree-node) "filter-result")))))
