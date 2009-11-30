;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; definition-source/inspector

(def (component e) definition-source/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null sb-introspect:definition-source) definition-source/inspector)

(def layered-method make-alternatives ((component definition-source/inspector) (class structure-class) (prototype sb-introspect:definition-source) (value sb-introspect:definition-source))
  (list* (delay-alternative-component-with-initargs 'definition-source/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; definition-source/lisp-form/inspector

(def (component e) definition-source/lisp-form/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component definition-source/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-definition-source-lisp-source component-value)))))
