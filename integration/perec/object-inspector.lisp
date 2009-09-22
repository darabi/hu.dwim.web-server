;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customizations

(def layered-method make-context-menu-items ((component standard-object-inspector) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) (instance hu.dwim.perec::persistent-object))
  (append (call-next-method)
          (optional-list (when (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::delete-entity-operation :-entity- class)
                           (make-delete-instance-command component class instance)))))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class hu.dwim.perec::persistent-class) (instance hu.dwim.perec::persistent-object))
  (remove-if #'hu.dwim.perec:persistent-object-internal-slot-p (call-next-method)))

(def layered-method collect-standard-object-detail-inspector-slots ((component standard-object-detail-inspector) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object))
  (filter-if (lambda (slot)
               (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::read-entity-property-operation :-entity- class :-property- slot))
             (call-next-method)))

(def layered-method make-standard-class-presentation ((component component) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object))
  (if (hu.dwim.meta-model::developer-p (hu.dwim.meta-model::current-effective-subject))
      (make-viewer class :initial-alternative-type 'reference-component)
      (call-next-method)))

(def layered-method delete-instance ((component standard-object-inspector) (class hu.dwim.perec::persistent-class) (instance hu.dwim.perec::persistent-object))
  (rdbms::with-transaction
    (hu.dwim.perec::purge-instance instance)
    (call-next-method)))

(def method join-editing ((self place-inspector))
  (bind ((place (place-of self)))
    (when (or (not (typep place 'object-slot-place))
              (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::write-entity-property-operation :-entity- (class-of (instance-of place)) :-property- (slot-of place)))
      (call-next-method))))
