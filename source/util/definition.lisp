;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Definition

(def (class* e) definition ()
  ((name :type symbol)
   (package :type package)
   (documentation :type (or null string))
   (source-file :type (or null pathname))))

(def (class* e) constant-definition (definition)
  ())

(def (class* e) variable-definition (definition)
  ())

(def (class* e) macro-definition (definition)
  ())

(def (class* e) function-definition (definition)
  ())

(def (class* e) generic-function-definition (definition)
  ())

(def (class* e) type-definition (definition)
  ())

(def (class* e) structure-definition (definition)
  ())

(def (class* e) condition-definition (definition)
  ())

(def (class* e) class-definition (definition)
  ())

(def (class* e) package-definition (definition)
  ())

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
