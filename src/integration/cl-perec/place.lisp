;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method slot-type ((slot prc::persistent-slot-definition))
  ;; TODO: KLUDGE: we cannot use canonical type because it does not allow dispatching on custom types (they are not in the canonical form)
  ;; unfortunately we cannot use specified type either because it may contain and, or, not functionals
  (bind ((specified-type (prc::specified-type-of slot)))
    (if (and (consp specified-type)
             (eq (first specified-type) 'and))
        (prc::canonical-type-of slot)
        specified-type)))

(def method slot-value-place-editable-p ((place slot-value-place) (class dmm::entity) (instance prc::persistent-object) (slot dmm::effective-property))
  (dmm::editable-p slot))

(def method slot-value-place-editable-p ((place slot-value-place) (class computed-class) (instance computed-object) (slot computed-effective-slot-definition))
  #f)
