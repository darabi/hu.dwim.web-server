;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method collect-standard-object-tree-table-inspector-slots ((component standard-object-tree-table-inspector) (class prc::persistent-class) instance)
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-tree-table-inspector-slots ((component standard-object-tree-table-inspector) (class dmm::entity) instance)
  (filter-if (lambda (slot)
               (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method make-standard-commands ((component standard-object-tree-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
    (make-editing-commands component)))

(def layered-method make-standard-commands ((component standard-object-tree-node-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
            (make-editing-commands component))
          (optional-list (when (dmm::authorize-operation 'expand-reference-operation :-entity- class)
                           (make-expand-node-command component instance)))))
