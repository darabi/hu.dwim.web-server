;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class hu.dwim.perec::persistent-class) (instance hu.dwim.perec::persistent-object))
  (remove-if #'hu.dwim.perec:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-list-table-inspector-slots ((component standard-object-list-table-inspector) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object))
  (filter-if (lambda (slot)
               (and (hu.dwim.meta-model::primary-p slot)
                    (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::read-entity-property-operation :-entity- class :-property- slot)))
             (call-next-method)))

(def layered-method make-begin-editing-new-instance-command ((component standard-object-list-inspector) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object))
  (when (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::create-instance-operation :-entity- class)
    (call-next-method)))

(def layered-method create-instance ((ancestor standard-object-list-inspector) (component standard-object-maker) (class hu.dwim.perec::persistent-class))
  (prog1-bind instance (call-next-method)
    (bind ((slot-value (find-ancestor-component-with-type ancestor 'standard-object-slot/mixin)))
      (when slot-value
        (bind ((slot (slot-of slot-value))
               (other-slot (hu.dwim.perec::other-association-end-of slot)))
          (setf (slot-value instance (slot-definition-name other-slot)) (instance-of slot-value)))))))
