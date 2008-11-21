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
