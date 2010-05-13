;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-slot-definition/alternator/inspector

(def (component e) standard-slot-definition/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null standard-slot-definition) standard-slot-definition/alternator/inspector)

(def layered-method make-alternatives ((component standard-slot-definition/alternator/inspector) (class standard-class) (prototype standard-slot-definition) (value standard-slot-definition))
  (list* (make-instance 'standard-slot-definition/documentation/inspector :component-value value)
         (make-instance 'standard-slot-definition/lisp-form/inspector :component-value value)
         (call-next-layered-method)))

;;;;;;
;;; standard-slot-definition/lisp-form/inspector

(def (component e) standard-slot-definition/lisp-form/inspector (t/detail/inspector)
  ())

;;;;;;
;;; standard-slot-definition/documentation/inspector

(def (component e) standard-slot-definition/documentation/inspector (t/documentation/inspector)
  ())

;;;;;;
;;; standard-slot-definition-sequence/table/inspector

(def (component e) standard-slot-definition-sequence/table/inspector (sequence/table/inspector)
  ())

(def layered-method make-row-presentation ((component standard-slot-definition-sequence/table/inspector) class prototype value)
  (make-instance 'standard-slot-definition/row/inspector
                 :component-value value
                 :edited (edited-component? component)
                 :editable (editable-component? component)))

(def layered-method collect-presented-slots ((component standard-slot-definition-sequence/table/inspector) class prototype value)
  (filter-slots '(sb-pcl::name sb-pcl::%type) (call-next-layered-method)))

;;;;;;
;;; standard-slot-definition/row/inspector

(def (component e) standard-slot-definition/row/inspector (t/row/inspector)
  ())
