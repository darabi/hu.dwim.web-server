;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; system/inspector

(def (component e) system/inspector (t/inspector)
  ())

(def (macro e) system/inspector (system &rest args &key &allow-other-keys)
  `(make-instance 'system/inspector :component-value ,system ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype asdf:system))
  'system/inspector)

(def layered-method make-alternatives ((component system/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (list* (delay-alternative-component-with-initargs 'system/depends-on-hierarchy/tree/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; system/depends-on-hierarchy/tree/inspector

(def (component e) system/depends-on-hierarchy/tree/inspector (t/tree/inspector)
  ())

(def (macro e) system/depends-on-hierarchy/tree/inspector (system &rest args &key &allow-other-keys)
  `(make-instance 'system/depends-on-hierarchy/tree/inspector :component-value ,system ,@args))

(def layered-method make-tree/root-node ((component system/depends-on-hierarchy/tree/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (make-instance 'system/depends-on-hierarchy/node/inspector :component-value value))

;;;;;;
;;; system/depends-on-hierarchy/node/inspector

(def (component e) system/depends-on-hierarchy/node/inspector (t/node/inspector)
  ())

(def (macro e) system/depends-on-hierarchy/node/inspector (system &rest args &key &allow-other-keys)
  `(make-instance 'system/depends-on-hierarchy/node/inspector :component-value ,system ,@args))

(def layered-method make-node/child-node ((component system/depends-on-hierarchy/node/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (make-instance 'system/depends-on-hierarchy/node/inspector :component-value value :expanded #f))

(def layered-method collect-tree/children ((component system/depends-on-hierarchy/node/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (mapcar #'asdf:find-system
          (cdr (find-if (lambda (description)
                          (eq 'asdf:load-op (first description)))
                        (asdf:component-depends-on 'asdf:load-op value)))))
