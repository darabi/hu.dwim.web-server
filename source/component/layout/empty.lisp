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
    (:documentation "An EMPTY/ABSTRACT COMPONENT is rendered completely empty, thus it is practically INVISIBLE. The reason to use EMPTY/ABSTRACT instead of NIL is to be able to make NIL an invalid COMPONENT. This approach helps debugging by popping up errors earlier. The base COMPONENT EMPTY/ABSTRACT does not have any state.")))

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
    (:documentation "To decrease memory footprint the EMPTY/LAYOUT/SINGLETON is used as a singleton class. It does not support PARENT-COMPONENT-OF and it does not have any state.")))

(def load-time-constant +empty-layout-singleton-instance+ (make-instance 'empty/layout/singleton))

(def (macro e) empty/layout/singleton ()
  '+empty-layout-singleton-instance+)

(def (function e) empty-layout? (component)
  (or (eq +empty-layout-singleton-instance+ component)
      (typep component 'empty/abstract)))

;;;;;;
;;; empty/layout

(def (component e) empty/layout (empty/abstract parent/mixin frame-unique-id/mixin)
  ()
  (:documentation "An EMPTY/LAYOUT may be used as part of the COMPONENT hierarchy. It supports PARENT-COMPONENT-OF, because it is often used as a placeholder by replacing it with another COMPONENT."))

(def (macro e) empty/layout ()
  '(make-instance 'empty/layout))

(def render-component empty/layout
  ;; NOTE: we do need the empty string in the body to workaround a bug in firefox
  <div (:id ,(id-of -self-)) "">)
