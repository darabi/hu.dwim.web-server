;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; system/inspector

(def (component e) system/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null asdf:system) system/inspector)

(def layered-method make-alternatives ((component system/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (list* (delay-alternative-component-with-initargs 'system/depends-on-hierarchy/tree/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; system/depends-on-hierarchy/tree/inspector

(def (component e) system/depends-on-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component system/depends-on-hierarchy/tree/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (make-instance 'system/depends-on-hierarchy/node/inspector :component-value value))

;;;;;;
;;; system/depends-on-hierarchy/node/inspector

(def (component e) system/depends-on-hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component system/depends-on-hierarchy/node/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (make-instance 'system/depends-on-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component system/depends-on-hierarchy/node/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (mapcar #'asdf:find-system
          (cdr (find-if (lambda (description)
                          (eq 'asdf:load-op (first description)))
                        (asdf:component-depends-on 'asdf:load-op value)))))
