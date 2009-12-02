;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; viewer/abstract

(def (component e) viewer/abstract (presentation/abstract)
  ()
  (:documentation "An VIEWER/ABSTRACT displays existing values of a TYPE.
  - similar to #<LITERAL-OBJECT {100C204081}>
  - static input
    - value-type: type
  - volatile input
    - value: value-type
  - output
    - value: value-type"))

;;;;;;
;;; viewer/minimal

(def (component e) viewer/minimal (viewer/abstract presentation/minimal)
  ())

;;;;;;
;;; viewer/basic

(def (component e) viewer/basic (viewer/minimal presentation/basic)
  ())

;;;;;;
;;; viewer/style

(def (component e) viewer/style (viewer/basic presentation/style)
  ())

;;;;;;
;;; viewer/full

(def (component e) viewer/full (viewer/style presentation/full)
  ())

;;;;;;
;;; Viewer factory

(def layered-method make-viewer :before (type &key (value nil value?) &allow-other-keys)
  (when (and value?
             (not (typep value type)))
    (error "Cannot make viewer for the value ~A~% which is not of type ~A" value type)))

(def layered-method make-viewer (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #f args))
