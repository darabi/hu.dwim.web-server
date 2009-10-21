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

(def (macro e) function/inspector ((&rest args &key &allow-other-keys) &body function)
  `(make-instance 'function/inspector ,@args :component-value ,(the-only-element function)))

(def layered-method find-inspector-type-for-prototype ((prototype function))
  ;; FIXME: workaround bug in SBCL? The value #<FUNCTION {1000DA3AC1}> is not of type FUNCTION.
  (declare (optimize (safety 0)))
  'function/inspector)

(def layered-method make-alternatives ((component function/inspector) class prototype value)
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
