;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def method slot-type ((slot hu.dwim.perec::persistent-slot-definition))
  ;; TODO: KLUDGE: we cannot use canonical type because it does not allow dispatching on custom types (they are not in the canonical form)
  ;; unfortunately we cannot use specified type either because it may contain and, or, not functionals
  (bind ((specified-type (hu.dwim.perec::specified-type-of slot)))
    (if (and (consp specified-type)
             (eq (first specified-type) 'and))
        (hu.dwim.perec::canonical-type-of slot)
        specified-type)))

(def method instance-slot-place-editable? ((place instance-slot-place) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object) (slot hu.dwim.meta-model::effective-property))
  (hu.dwim.meta-model::editable-p slot))

(def method instance-slot-place-editable? ((place instance-slot-place) (class computed-class) (instance computed-object) (slot computed-effective-slot-definition))
  #f)
