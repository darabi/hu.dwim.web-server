;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; test/inspector

(def (component e) test/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null hu.dwim.stefil::test) test/inspector)

(def layered-method make-alternatives ((component test/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (list* (make-instance 'test/hierarchy/tree/inspector :component-value value)
         (make-instance 'test/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; test/lisp-form/inspector

(def (component e) test/lisp-form/inspector (inspector/style content/widget)
  ())

(def refresh-component test/lisp-form/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (make-instance 'function/lisp-form/inspector :component-value (symbol-function (hu.dwim.stefil::name-of component-value))))))

;;;;;;
;;; test/hierarchy/tree/inspector

(def (component e) test/hierarchy/tree/inspector (t/tree/inspector)
  ())

(def layered-method make-tree/root-node ((component test/hierarchy/tree/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value))

;;;;;;
;;; test/hierarchy/node/inspector

(def (component e) test/hierarchy/node/inspector (t/node/inspector)
  ())

(def layered-method make-node/child-node ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method make-node/content ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-value-inspector value
                        :initial-alternative-type 't/reference/inspector
                        :default-alternative-type 'test/lisp-form/inspector
                        :edited (edited-component? component)
                        :editable (editable-component? component)))

(def layered-method collect-tree/children ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (sort (hash-table-values (hu.dwim.stefil::children-of value)) #'string< :key #'hu.dwim.stefil::name-of))
