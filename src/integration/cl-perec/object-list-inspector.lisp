;;; Copyright (c) 2003-2009 by the authors.
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

(def layered-method make-begin-editing-new-instance-command ((component standard-object-list-inspector) (class dmm::entity) (instance prc::persistent-object))
  (when (dmm::authorize-operation 'dmm::create-instance-operation :-entity- class)
    (call-next-method)))

(def layered-method create-instance ((ancestor standard-object-list-inspector) (component standard-object-maker) (class prc::persistent-class))
  (prog1-bind instance (call-next-method)
    (bind ((slot-value (find-ancestor-component-with-type ancestor 'standard-object-slot/mixin)))
      (when slot-value
        (bind ((slot (slot-of slot-value))
               (other-slot (prc::other-association-end-of slot)))
          (setf (slot-value instance (slot-definition-name other-slot)) (instance-of slot-value)))))))
