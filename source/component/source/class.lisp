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
  (bind (((:read-only-slots editable-component edited-component component-value-type) component))
    (list* (make-instance 'class/documentation/inspector
                          :component-value value
                          :component-value-type component-value-type
                          :edited edited-component
                          :editable editable-component)
           (make-instance 'class/lisp-form/inspector
                          :component-value value
                          :component-value-type component-value-type
                          :edited edited-component
                          :editable editable-component)
           (make-instance 'class/subclass-hierarchy/tree/inspector
                          :component-value value
                          :component-value-type component-value-type
                          :edited edited-component
                          :editable editable-component)
           (make-instance 'class/superclass-hierarchy/tree/inspector
                          :component-value value
                          :component-value-type component-value-type
                          :edited edited-component
                          :editable editable-component)
           (call-next-layered-method))))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) prototype (value class))
  (string+ (localized-class-name class :capitalize-first-letter #t) ": " (call-next-layered-method)))

;;;;;;
;;; class/lisp-form/inspector

(def (component e) class/lisp-form/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component class/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (definition-source-text component-value)))))

;;;;;;
;;; class/documentation/inspector

(def (component e) class/documentation/inspector (t/documentation/inspector title/mixin)
  ((direct-slots :type component)))

(def refresh-component class/documentation/inspector
  (bind (((:slots direct-slots component-value) -self-))
    (setf direct-slots (make-instance 'standard-slot-definition-sequence/table/inspector :component-value (class-direct-slots component-value)))))

(def render-component class/documentation/inspector
  (render-title-for -self-)
  (render-contents-for -self-)
  (render-component (direct-slots-of -self-)))

(def render-xhtml class/documentation/inspector
  (with-render-style/abstract (-self-)
    (render-title-for -self-)
    (render-contents-for -self-)
    (render-component (direct-slots-of -self-))))

(def layered-method make-title ((self class/documentation/inspector) (class standard-class) (prototype standard-class) (value standard-class))
  (title/widget ()
    (localized-class-name value :capitalize-first-letter #t)))

;;;;;;
;;; class/subclass-hierarchy/tree/inspector

(def (component e) class/subclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-node-presentation ((component class/subclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/subclass-hierarchy/node/inspector

(def (component e) class/subclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node-presentation ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-presented-children ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (copy-list (class-direct-subclasses value)) #'string< :key (compose #'fully-qualified-symbol-name #'class-name)))

;;;;;;
;;; class/superclass-hierarchy/tree/inspector

(def (component e) class/superclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-node-presentation ((component class/superclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/superclass-hierarchy/node/inspector

(def (component e) class/superclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node-presentation ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-presented-children ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (copy-list (class-direct-superclasses value)) #'string< :key (compose #'fully-qualified-symbol-name #'class-name)))

;;;;;;
;;; class/tree-level/inspector

(def (component e) class/tree-level/inspector (t/tree-level/inspector)
  ())

(def layered-method make-path-presentation ((component class/tree-level/inspector) (class class) (prototype class) (value list))
  (make-instance 'class/tree-level/path/inspector :component-value value))

(def layered-method make-previous-sibling-presentation ((component class/tree-level/inspector) (class class) (prototype class) value)
  (make-instance 'class/tree-level/reference/inspector :component-value value))

(def layered-method make-next-sibling-presentation ((component class/tree-level/inspector) (class class) (prototype class) value)
  (make-instance 'class/tree-level/reference/inspector :component-value value))

(def layered-method make-descendants-presentation ((component class/tree-level/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/tree/inspector :component-value value))

(def layered-method make-node-presentation ((component class/tree-level/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; class/tree-level/reference/inspector

(def (component e) class/tree-level/reference/inspector (t/reference/inspector)
  ())

(def refresh-component class/tree-level/reference/inspector
  (bind (((:slots content action component-value) -self-))
    (setf content (fully-qualified-symbol-name (class-name component-value)))
    (setf action (make-action
                   (setf (component-value-of (find-ancestor-component-of-type 'class/tree-level/inspector -self-)) component-value)))))

;;;;;;
;;; class/tree-level/path/inspector

(def (component e) class/tree-level/path/inspector (t/tree-level/path/inspector)
  ())

(def method component-dispatch-class ((self class/tree-level/path/inspector))
  (class-of (first (component-value-of self))))

(def layered-method make-content-presentation ((component class/tree-level/path/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/reference/inspector :component-value value))

;;;;;;
;;; class/tree-level/tree/inspector

(def (component e) class/tree-level/tree/inspector (class/subclass-hierarchy/tree/inspector)
  ())

(def layered-method make-node-presentation ((component class/tree-level/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/node/inspector :component-value value))

;;;;;;
;;; class/tree-level/node/inspector

(def (component e) class/tree-level/node/inspector (class/subclass-hierarchy/node/inspector)
  ())

(def layered-method make-node-presentation ((component class/tree-level/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/tree-level/node/inspector :component-value value))

(def layered-method make-content-presentation ((component class/tree-level/node/inspector) (class class) (prototype class) (value class))
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
