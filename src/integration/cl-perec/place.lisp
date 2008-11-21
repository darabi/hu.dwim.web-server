;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

(def method slot-type ((slot prc::persistent-slot-definition))
  (prc::specified-type-of slot))

(def method slot-value-place-editable-p ((place slot-value-place) (class dmm::entity) (instance prc::persistent-object) (slot dmm::effective-property))
  (dmm::editable-p slot))

(def method slot-value-place-editable-p ((place slot-value-place) (class computed-class) (instance computed-object) (slot computed-effective-slot-definition))
  #f)
