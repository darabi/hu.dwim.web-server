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

(def subtype-mapper *inspector-type-mapping* (or null definition) definition/inspector)

(def layered-method make-alternatives ((component definition/inspector) (class standard-class) (prototype definition) (value definition))
  (list* (delay-alternative-component-with-initargs 'definition/lisp-form/inspector :component-value value)
         (delay-alternative-component-with-initargs 'definition/documentation/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; t/reference/inspector

(def function make-definition-class-name (class)
  (trim-suffix "-DEFINITION" (symbol-name (class-name class))))

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value definition))
  (string+ (capitalize-first-letter (string-downcase (make-definition-class-name class)))  ": " (string-upcase (name-of value))))

(def layered-method make-reference-content ((component t/reference/inspector) class prototype (value class-definition))
  (string+ (localized-class-name (class-of (find-class (name-of value))) :capitalize-first-letter #t) ": " (string-upcase (name-of value))))

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
  (:method ((component definition/lisp-form/inspector) class prototype (value definition))
    (bind ((name (make-definition-class-name class))
           (source (definition-source-text (first (sb-introspect:find-definition-sources-by-name (name-of value) (intern (string-upcase name) :keyword))))))
      (make-instance 't/lisp-form/inspector :component-value source)))

  (:method ((component definition/lisp-form/inspector) class prototype (value macro-definition))
    (make-instance 't/lisp-form/inspector :component-value (definition-source-text (macro-function (name-of value)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value function-definition))
    (make-instance 't/lisp-form/inspector :component-value (definition-source-text (symbol-function (name-of value)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value generic-function-definition))
    (make-instance 'standard-method-sequence/lisp-form-list/inspector :component-value (generic-function-methods (symbol-function (name-of value)))))

  (:method ((component definition/lisp-form/inspector) class prototype (value class-definition))
    (make-instance 'class/lisp-form/inspector :component-value (find-class (name-of value)))))

;;;;;;
;;; definition/documentation/inspector

(def (component e) definition/documentation/inspector (t/documentation/inspector)
  ())

(def method make-documentation ((component definition/documentation/inspector) (class standard-class) (prototype definition) (value definition))
  (documentation-of value))

;;;;;;
;;; t/filter

(def layered-method map-filter-input ((component t/filter) (class standard-class) (prototype definition) (value standard-class) function)
  (bind ((seen-set (make-hash-table :test #'eq)))
    (do-all-symbols (name)
      (unless (gethash name seen-set)
        (foreach function (make-definitions name))
        (setf (gethash name seen-set) #t)))))

;;;;;;
;;; Util

(def function make-definitions (name)
  (macrolet ((make (class-name documentation)
               `(make-instance ,class-name
                               :name name
                               :package package
                               :documentation ,documentation
                               :source-file pathname)))
    (iter outer
          (with package = (symbol-package name))
          (for type :in swank-backend::*definition-types* :by #'cddr)
          ;; KLUDGE: remove ignore-errors as soon as this does not error out (sb-introspect:find-definition-sources-by-name 'common-lisp:structure-object :structure)
          (iter (for specification :in (ignore-errors (sb-introspect:find-definition-sources-by-name name type)))
                (for pathname = (sb-introspect::definition-source-pathname specification))
                (awhen (case type
                         (:constant (make 'constant-definition (documentation name 'variable)))
                         (:variable (make 'variable-definition (documentation name 'variable)))
                         (:macro (make 'macro-definition (documentation (macro-function name) 'function)))
                         (:function (make 'function-definition (documentation (symbol-function name) 'function)))
                         (:generic-function (make 'generic-function-definition (documentation (symbol-function name) 'function)))
                         (:type (make 'type-definition (documentation name 'type)))
                         (:structure (make 'structure-definition (documentation name 'type)))
                         (:condition (make 'condition-definition (documentation (find-class name) 'type)))
                         (:class (make 'class-definition (documentation (find-class name) 'type)))
                         (:package (make 'package-definition (documentation (find-package name) t)))
                         (:method nil)
                         (:compiler-macro nil)
                         (:method-combination nil)
                         (:setf-expander nil)
                         (:symbol-macro nil)
                         (t nil))
                  (in outer (collect it)))))))
