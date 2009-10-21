;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; definition/inspector

(def (component e) definition/inspector (t/inspector)
  ())

(def layered-method find-inspector-type-for-prototype ((prototype definition))
  'definition/inspector)

(def layered-method make-alternatives ((component definition/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'definition/lisp-form/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value special-variable-definition))
  (string+ "Special variable: " (string-upcase (name-of value))))

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value macro-definition))
  (string+ "Macro: " (string-upcase (name-of value))))

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value function-definition))
  (string+ "Function: " (string-upcase (name-of value))))

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value generic-function-definition))
  (string+ "Generic function: " (string-upcase (name-of value))))

;;;;;;
;;; definition/lisp-form/inspector

(def (component e) definition/lisp-form/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component definition/lisp-form/inspector
  (bind (((:slots component-value content) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-)))
    (setf content (make-definition/lisp-form/content -self- dispatch-class dispatch-prototype component-value))))

(def generic make-definition/lisp-form/content (component class prototype value)
  (:method ((component definition/lisp-form/inspector) class prototype (value special-variable-definition))
    (make-instance 't/lisp-form/inspector :component-value (read-definition-source-lisp-source (first (sb-introspect:find-definition-sources-by-name (name-of value) :variable)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value macro-definition))
    (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source (macro-function (name-of value)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value function-definition))
    (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source (symbol-function (name-of value)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value generic-function-definition))
    (make-instance 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods (symbol-function (name-of value))))))
