;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method make-standard-object-inspector-commands ((component standard-object-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (append (when (dmm::authorize-operation 'dmm::write-entity-operation :-entity- class)
            (make-editing-commands component))
          (optional-list (when (dmm::authorize-operation 'dmm::delete-entity-operation :-entity- class)
                           (make-delete-instance-command component)))))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class dmm::entity) (instance prc::persistent-object))
  (filter-if (lambda (slot)
               (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method make-class-presentation ((component component) (class prc::persistent-class) (prototype prc::persistent-object))
  (if (dmm::developer-p (dmm::current-effective-subject))
      (make-viewer class :default-component-type 'reference-component)
      (call-next-method)))

(def layered-method execute-delete-instance ((component standard-object-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (rdbms::with-transaction
    (prc::purge-instance instance)
    (call-next-method)))
