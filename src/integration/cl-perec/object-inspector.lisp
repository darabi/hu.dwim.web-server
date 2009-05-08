;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method make-context-menu-commands ((component standard-object-inspector) (class prc::persistent-class) (prototype prc::persistent-object) (instance prc::persistent-object))
  (append (call-next-method)
          (optional-list (when (dmm::authorize-operation 'dmm::delete-entity-operation :-entity- class)
                           (make-delete-instance-command component class instance)))))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (remove-if #'prc:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class dmm::entity) (instance prc::persistent-object))
  (filter-if (lambda (slot)
               (dmm::authorize-operation 'dmm::read-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method make-class-presentation ((component component) (class prc::persistent-class) (prototype prc::persistent-object))
  (if (dmm::developer-p (dmm::current-effective-subject))
      (make-viewer class :initial-alternative-type 'reference-component)
      (call-next-method)))

(def layered-method execute-delete-instance ((component standard-object-inspector) (class prc::persistent-class) (instance prc::persistent-object))
  (rdbms::with-transaction
    (prc::purge-instance instance)
    (call-next-method)))

(def method join-editing ((self place-inspector))
  (bind ((place (place-of self)))
    (when (or (not (typep place 'slot-value-place))
              (dmm::authorize-operation 'dmm::write-entity-property-operation :-entity- (class-of (instance-of place)) :-property- (slot-of place)))
      (call-next-method))))
