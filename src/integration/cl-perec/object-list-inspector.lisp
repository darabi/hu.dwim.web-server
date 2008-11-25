;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class dmm::entity) (instance prc::persistent-object))
  (filter-if (lambda (slot)
               (and (dmm::primary-p slot)
                    (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot)))
             (call-next-method)))

(def layered-method make-expand-command ((component standard-object-row-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (when (dmm::authorize-operation 'expand-instance-operation :-entity- class)
    (call-next-method)))

(def layered-method make-editing-commands ((component abstract-standard-object-list-component) (class prc::persistent-class) (instance-or-prototype prc::persistent-object))
  (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
    (call-next-method)))
