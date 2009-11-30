;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; function/inspector

(def (component e) function/inspector (t/inspector)
  ())

(def subtype-mapper *inspector-type-mapping* (or null function) function/inspector)

(def layered-method make-alternatives ((component function/inspector) (class built-in-class) (prototype function) (value function))
  (list* (delay-alternative-component-with-initargs 'function/lisp-form/inspector :component-value value)
         (call-next-method)))

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
             ": " (call-next-method))))

;;;;;;
;;; function/lisp-form/inspector

(def (component e) function/lisp-form/inspector (inspector/basic t/detail/presentation content/widget)
  ())

(def refresh-component function/lisp-form/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (make-instance 't/lisp-form/inspector :component-value (read-definition-lisp-source component-value)))))

;;;;;;
;;; t/filter

(def layered-method map-filter-input ((component t/filter) (class standard-class) (prototype built-in-class) (value (eql (find-class 'function))) function)
  (do-all-symbols (name)
    (when (fboundp name)
      (funcall function (symbol-function name)))))
