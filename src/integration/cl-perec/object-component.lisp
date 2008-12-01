;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method reuse-standard-object-instance ((class prc::persistent-class) (instance prc::persistent-object))
  (if (prc::persistent-p instance)
      (prc::load-instance instance)
      (call-next-method)))

(def method hash-key-for ((instance prc::persistent-object))
  (prc::oid-of instance))

(def layered-method make-expand-command ((component standard-object-tree-node-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (when (dmm::authorize-operation 'expand-instance-operation :-entity- class)
    (call-next-method)))

(def layered-method collect-standard-object-detail-slot-groups ((component standard-object-detail-component) (class dmm::entity) (prototype prc::persistent-object) (slots list))
  (bind ((slot-groups (partition slots #'dmm::primary-p (constantly #t))))
    (list (cons #"standard-object-detail-component.primary-group" (first slot-groups))
          (cons #"standard-object-detail-component.secondary-group" (second slot-groups)))))
