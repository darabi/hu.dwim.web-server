;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Customization

(def layered-method make-editing-commands ((component editable/mixin) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) (instance hu.dwim.perec::persistent-object))
  (when (hu.dwim.meta-model::authorize-operation 'hu.dwim.meta-model::write-entity-operation :-entity- class)
    (call-next-method)))
