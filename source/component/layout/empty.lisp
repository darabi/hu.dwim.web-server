;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; empty/layout

(eval-always
  ;; NOTE: Do not include parent/mixin in the superclasses, because does not work with being a singleton.
  ;;       The empty/layout supposed to be the substitute for NIL which is not a valid component.
  (def (component e) empty/layout (layout/abstract)
    ()
    (:documentation "An EMPTY/LAYOUT is completely empty, it is practically INVISIBLE. For performance reasons it is used as a singleton, and it does not support PARENT-COMPONENT-OF. The reason to use EMPTY/LATOUT instead of NIL is to make NIL an invalid COMPONENT for debugging purposes. The component EMPTY/LAYOUT does not have any state.")))

(def load-time-constant +empty-layout-singleton-instance+ (make-instance 'empty/layout))

(def (macro e) empty/layout ()
  '+empty-layout-singleton-instance+)

(def render-component empty/layout
  (values))

(def (function e) empty-layout? (component)
  (eq +empty-layout-singleton-instance+ component))
