;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; empty/abstract

(eval-always
  (def (component e) empty/abstract (layout/abstract)
    ()
    (:documentation "An EMPTY/ABSTRACT is completely empty, it is practically INVISIBLE. The reason to use EMPTY/ABSTRACT instead of NIL, is to be able to make NIL an invalid COMPONENT for debugging purposes. The base component EMPTY/ABSTRACT does not have any state.")))

(def render-component empty/abstract
  (values))

;;;;;;
;;; empty/layout/singleton

(eval-always
  ;; NOTE: Do not include parent/mixin in the superclasses, because that does not work with being a singleton.
  ;; The empty/layout/singleton is the substitute for NIL, because NIL is not a valid component.
  ;; The number of effective slots in this class supposed to be zero.
  (def (component e) empty/layout/singleton (empty/abstract)
    ()
    (:documentation "To save memory an EMPTY/LAYOUT/SINGLETON is used as a singleton, and it does not support PARENT-COMPONENT-OF.")))

(def load-time-constant +empty-layout-singleton-instance+ (make-instance 'empty/layout/singleton))

(def (macro e) empty/layout/singleton ()
  '+empty-layout-singleton-instance+)

(def (function e) empty-layout? (component)
  (or (eq +empty-layout-singleton-instance+ component)
      (typep component 'empty/abstract)))

;;;;;;
;;; empty/layout

(def (component e) empty/layout (empty/abstract parent/mixin)
  ()
  (:documentation "An EMPTY/LAYOUT may be used as part of the COMPONENT hierarchy, that is it supports PARENT-COMPONENT-OF."))

(def (macro e) empty/layout ()
  '(make-instance 'empty/layout))
