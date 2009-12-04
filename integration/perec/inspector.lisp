;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def subtype-mapper *inspector-type-mapping* (hu.dwim.perec::set hu.dwim.perec::persistent-object) sequence/inspector)

(def method object-slot-place-editable? ((place object-slot-place) (class hu.dwim.meta-model::entity) (instance hu.dwim.perec::persistent-object) (slot hu.dwim.meta-model::effective-property))
  (hu.dwim.meta-model::editable-p slot))
