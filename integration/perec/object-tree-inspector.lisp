;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method collect-standard-object-tree-table-inspector-slots ((component standard-object-tree-table-inspector) (class hu.dwim.perec::persistent-class) instance)
  (remove-if #'hu.dwim.perec:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-tree-table-inspector-slots ((component standard-object-tree-table-inspector) (class hu.dwim.meta-model::entity) instance)
  (filter-if (lambda (slot)
               (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::read-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))
