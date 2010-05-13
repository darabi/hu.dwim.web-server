;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; function/alternator/inspector

(def (component e) function/alternator/inspector (t/alternator/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null function) function/alternator/inspector)

(def layered-method make-alternatives ((component function/alternator/inspector) (class built-in-class) (prototype function) (value function))
  (list* (make-instance 'function/documentation/inspector :component-value value)
         (make-instance 'function/lisp-form/inspector :component-value value)
         (call-next-layered-method)))

;;;;;;
;;; t/reference/inspector

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value function))
  (bind ((name (function-name value)))
    (string+ (cond ((symbolp name)
                    (localized-class-name class :capitalize-first-letter #t))
                   ((and (consp name)
                         (eq (first name) 'macro-function))
                    "Macro")
                   (t "Unknown"))
             ": " (call-next-layered-method))))

;;;;;;
;;; function/documentation/inspector

(def (component e) function/documentation/inspector (t/documentation/inspector)
  ())

(def method make-documentation ((component function/documentation/inspector) (class standard-class) (prototype function) (value function))
  (documentation value t))

;;;;;;
;;; function/lisp-form/inspector

(def (component e) function/lisp-form/inspector (t/detail/inspector content/widget)
  ())

(def refresh-component function/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (definition-source-text component-value)))))

;;;;;;
;;; t/filter

(def layered-method map-filter-input ((component t/filter) (class standard-class) (prototype built-in-class) (value (eql (find-class 'function))) function)
  (do-all-symbols (name)
    (when (fboundp name)
      (funcall function (symbol-function name)))))
