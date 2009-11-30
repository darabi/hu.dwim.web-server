;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; class/inspector

(def (component e) class/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null class) class/inspector)

(def layered-method make-alternatives ((component class/inspector) (class standard-class) (prototype class) (value class))
  (list* (delay-alternative-component-with-initargs 'class/subclass-hierarchy/tree/inspector :component-value value)
         (delay-alternative-component-with-initargs 'class/superclass-hierarchy/tree/inspector :component-value value)
         (delay-alternative-component-with-initargs 'class/lisp-form/inspector :component-value value)
         (delay-alternative-component-with-initargs 'class/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) prototype (value class))
  (string+ (localized-class-name class :capitalize-first-letter #t) ": " (call-next-method)))

;;;;;;
;;; class/lisp-form/inspector

(def (component e) class/lisp-form/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component class/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source component-value)))))

;;;;;;
;;; class/documentation/inspector

(def (component e) class/documentation/inspector (t/documentation/inspector)
  ())

;;;;;;
;;; class/subclass-hierarchy/tree/inspector

(def (component e) class/subclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component class/subclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/subclass-hierarchy/node/inspector

(def (component e) class/subclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (class-direct-subclasses value) #'string< :key (compose #'qualified-symbol-name #'class-name)))

;;;;;;
;;; class/superclass-hierarchy/tree/inspector

(def (component e) class/superclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component class/superclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/superclass-hierarchy/node/inspector

(def (component e) class/superclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (class-direct-superclasses value) #'string< :key (compose #'qualified-symbol-name #'class-name)))

;;;;;;
;;; class/tree-level/inspector

(def (component e) class/tree-level/inspector (t/tree-level/inspector)
  ())

(def layered-method make-tree-level/path ((component class/tree-level/inspector) (class class) (prototype class) (value list))
  (make-instance 'class/tree-level/path/inspector :component-value value))

(def layered-method make-tree-level/previous-sibling ((component class/tree-level/inspector) (class class) (prototype class) value)
  (make-instance 'class/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/next-sibling ((component class/tree-level/inspector) (class class) (prototype class) value)
  (make-instance 'class/tree-level/reference/inspector :component-value value))

(def layered-method make-tree-level/descendants ((component class/tree-level/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/tree/inspector :component-value value))

(def layered-method make-tree-level/node ((component class/tree-level/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; class/tree-level/reference/inspector

(def (component e) class/tree-level/reference/inspector (t/reference/inspector)
  ())

(def refresh-component class/tree-level/reference/inspector
  (bind (((:slots content action component-value) -self-))
    (setf content (qualified-symbol-name (class-name component-value)))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-with-type -self- 'class/tree-level/inspector)) component-value)))))

;;;;;;
;;; class/tree-level/path/inspector

(def (component e) class/tree-level/path/inspector (t/tree-level/path/inspector)
  ())

(def method component-dispatch-class ((self class/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-path/content ((component class/tree-level/path/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; class/tree-level/tree/inspector

(def (component e) class/tree-level/tree/inspector (class/subclass-hierarchy/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component class/tree-level/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/node/inspector :component-value value))

;;;;;;
;;; class/tree-level/node/inspector

(def (component e) class/tree-level/node/inspector (class/subclass-hierarchy/node/inspector)
  ())

(def layered-method make-node/child-node ((component class/tree-level/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/node/inspector :component-value value))

(def layered-method make-node/content ((component class/tree-level/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; t/filter

(def method slot-type (class (prototype class) slot)
  (case (slot-definition-name slot)
    (sb-pcl::name 'symbol)
    (sb-pcl::%documentation '(or null string))
    (t (call-next-method))))

(def layered-method map-filter-input ((component t/filter) (class class) (prototype class) (value class) function)
  (maphash-keys (lambda (key)
                  (awhen (find-class key #f)
                    (funcall function it)))
                sb-kernel::*classoid-cells*))
