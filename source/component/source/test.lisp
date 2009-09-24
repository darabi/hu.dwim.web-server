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

(def (macro e) test/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/inspector :component-value ,test ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype hu.dwim.stefil::test))
  'test/inspector)

(def layered-method make-alternatives ((component test/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (list* (delay-alternative-component-with-initargs 'test/hierarchy/tree/inspector :component-value value)
         (delay-alternative-component-with-initargs 'test/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; test/lisp-form/inspector

(def (component e) test/lisp-form/inspector (inspector/style content/widget)
  ())

(def refresh-component test/lisp-form/inspector
  (bind (((:slots content component-value) -self-))
    (setf content (make-instance 'function/lisp-form/inspector :component-value (fdefinition (hu.dwim.stefil::name-of component-value))))))

;;;;;;
;;; test/hierarchy/tree/inspector

(def (component e) test/hierarchy/tree/inspector (t/tree/inspector)
  ())

(def (macro e) test/hierarchy/tree/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/hierarchy/tree/inspector :component-value ,test ,@args))

(def layered-method make-tree/root-node ((component test/hierarchy/tree/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value))

;;;;;;
;;; test/hierarchy/node/inspector

(def (component e) test/hierarchy/node/inspector (t/node/inspector)
  ())

(def (macro e) test/hierarchy/node/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/hierarchy/node/inspector :component-value ,test ,@args))

(def layered-method make-node/child-node ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method make-node/content ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (make-value-inspector value :default-alternative-type 'test/lisp-form/inspector))

(def layered-method collect-tree/children ((component test/hierarchy/node/inspector) (class standard-class) (prototype hu.dwim.stefil::test) (value hu.dwim.stefil::test))
  (sort (hash-table-values (hu.dwim.stefil::children-of value)) #'string< :key #'hu.dwim.stefil::name-of))
