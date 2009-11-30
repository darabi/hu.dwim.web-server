;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; editor/abstract

(def (component e) editor/abstract (presentation/abstract)
  ())

;;;;;;
;;; editor/minimal

(def (component e) editor/minimal (editor/abstract presentation/minimal)
  ())

;;;;;;
;;; editor/basic

(def (component e) editor/basic (editor/minimal presentation/basic)
  ())

;;;;;;
;;; editor/style

(def (component e) editor/style (editor/basic presentation/style)
  ())

;;;;;;
;;; editor/full

(def (component e) editor/full (editor/style presentation/full)
  ())

;;;;;;
;;; Editor factory

(def layered-method make-editor :before (type &key (value nil value?) &allow-other-keys)
  (when (and value?
             (not (typep value type)))
    (error "Cannot make editor for the value ~A~% which is not of type ~A" value type)))

(def layered-method make-editor (type &rest args &key &allow-other-keys)
  (apply #'make-inspector type :editable #f :edited #t args))
