;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; inspector/abstract

(def (component e) inspector/abstract (presentation/abstract editable/mixin)
  ())

;;;;;;
;;; inspector/minimal

(def (component e) inspector/minimal (inspector/abstract presentation/minimal)
  ())

;;;;;;
;;; inspector/basic

(def (component e) inspector/basic (inspector/minimal presentation/basic)
  ())

;;;;;;
;;; inspector/style

(def (component e) inspector/style (inspector/basic presentation/style)
  ())

;;;;;;
;;; inspector/full

(def (component e) inspector/full (inspector/style presentation/full)
  ())

;;;;;;
;;; Inspector factory


;;;;;;
;;; TODO: steps to create an inspector
;;;
;;; 0. register all components based on the type they can inspect
;;; 1. go through and collect all components registered with subtypep
;;; 2. filter for those which are needed
;;; 3. if there's only only one alternative use that
;;; 4. find the most specific type that will be the default alternative and use an alternatork

(def special-variable *inspector-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *inspector-type-mapping* nil nil)

(def (layered-function e) find-inspector-type (type)
  (:method (type)
    (linear-mapping-value *inspector-type-mapping* type)))

(def layered-method make-inspector :before (type &key (value nil value?) &allow-other-keys)
  (when (and value?
             (not (typep value type)))
    (error "Cannot make inspector for the value ~A~% which is not of type ~A" value type)))

(def layered-method make-inspector (type &rest args &key value &allow-other-keys)
  (bind ((component-type (find-inspector-type type)))
    (apply #'make-instance component-type
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs (find-class component-type) args))))
