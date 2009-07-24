;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; test/inspector

(def (component e) test/inspector (t/inspector)
  ())

(def (macro e) test/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/inspector :component-value ,test ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype stefil::test))
  'test/inspector)

(def layered-method make-alternatives ((component test/inspector) (class standard-char) (prototype stefil::test) (value stefil::test))
  (list* (delay-alternative-component-with-initargs 'test/hierarchy/tree/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; test/hierarchy/tree/inspector

(def (component e) test/hierarchy/tree/inspector (t/tree/inspector)
  ())

(def (macro e) test/hierarchy/tree/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/hierarchy/tree/inspector :component-value ,test ,@args))

(def layered-method make-tree/root-node ((component test/hierarchy/tree/inspector) (class standard-class) (prototype stefil::test) (value stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value))

;;;;;;
;;; test/hierarchy/node/inspector

(def (component e) test/hierarchy/node/inspector (t/node/inspector)
  ())

(def (macro e) test/hierarchy/node/inspector (test &rest args &key &allow-other-keys)
  `(make-instance 'test/hierarchy/node/inspector :component-value ,test ,@args))

(def layered-method make-node/child-node ((component test/hierarchy/node/inspector) (class standard-class) (prototype stefil::test) (value stefil::test))
  (make-instance 'test/hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component test/hierarchy/node/inspector) (class standard-class) (prototype stefil::test) (value stefil::test))
  (sort (hash-table-values (stefil::children-of value)) #'string< :key #'stefil::name-of))
