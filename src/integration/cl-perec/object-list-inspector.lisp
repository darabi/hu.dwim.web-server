;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method make-standard-object-list-inspector-commands ((component standard-object-list-inspector) (class prc::persistent-class) (prototype prc::persistent-object))
  (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
    (make-editing-commands component)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class dmm::entity) (instance prc::persistent-object))
  (filter-if #'dmm::primary-p (call-next-method)))

(def layered-method make-standard-object-row-inspector-commands ((component standard-object-row-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
            (make-editing-commands component))
          (optional-list (when (dmm::authorize-operation 'expand-reference-operation :-entity- class)
                           (make-expand-row-command component instance)))))
