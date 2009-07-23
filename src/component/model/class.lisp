;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; class/inspector

(def (component e) class/inspector (t/inspector)
  ())

(def (macro e) class/inspector (class &rest args)
  `(make-instance 'class/inspector ,class ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype class))
  'class/inspector)

(def layered-method make-alternatives ((component class/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'class/subclass-hierarchy/tree/inspector :component-value value)
         (delay-alternative-component-with-initargs 'class/superclass-hierarchy/tree/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; class/subclass-hierarchy/tree/inspector

(def (component e) class/subclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def (macro e) class/subclass-hierarchy/tree/inspector (class &rest args)
  `(make-instance 'class/subclass-hierarchy/tree/inspector :component-value ,class ,@args))

(def layered-method make-tree/root-node ((component class/subclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/subclass-hierarchy/node/inspector

(def (component e) class/subclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def (macro e) class/subclass-hierarchy/node/inspector (class &rest args)
  `(make-instance 'class/subclass-hierarchy/node/inspector :component-value ,class ,@args))

(def layered-method make-node/child-node ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/subclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (class-direct-subclasses value) #'string< :key (compose #'qualified-symbol-name #'class-name)))

(def layered-method make-node/content ((component class/subclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-value-inspector value))

;;;;;;
;;; class/superclass-hierarchy/tree/inspector

(def (component e) class/superclass-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def (macro e) class/superclass-hierarchy/tree/inspector (class &rest args)
  `(make-instance 'class/superclass-hierarchy/tree/inspector :component-value ,class ,@args))

(def layered-method make-tree/root-node ((component class/superclass-hierarchy/tree/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value))

;;;;;;
;;; class/superclass-hierarchy/node/inspector

(def (component e) class/superclass-hierarchy/node/inspector (t/node/inspector)
  ())

(def (macro e) class/superclass-hierarchy/node/inspector (class &rest args)
  `(make-instance 'class/superclass-hierarchy/node/inspector :component-value ,class ,@args))

(def layered-method make-node/child-node ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-instance 'class/superclass-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (sort (class-direct-superclasses value) #'string< :key (compose #'qualified-symbol-name #'class-name)))

(def layered-method make-node/content ((component class/superclass-hierarchy/node/inspector) (class class) (prototype class) (value class))
  (make-value-inspector value))
