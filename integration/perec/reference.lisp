;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def (hu.dwim.meta-model:operation e) expand-instance-operation (hu.dwim.meta-model::standard-operation)
  ())

(def layered-method make-expand-reference-command ((reference reference-component) (class hu.dwim.meta-model::entity) target expansion)
  (if (hu.dwim.meta-model::authorize-operation 'expand-instance-operation :-entity- class)
      (call-next-method)
      (make-reference-label reference class target)))

(def method make-reference-label ((reference standard-object-inspector-reference) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object))
  (localized-instance-name (target-of reference)))

(def method make-reference-label ((reference standard-object-list-inspector-reference) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object))
  (localized-instance-name instance))
