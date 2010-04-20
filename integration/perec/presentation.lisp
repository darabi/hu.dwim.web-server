;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method collect-presented-slots :around (component (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) value)
  (remove-slots '(hu.dwim.perec::oid
                  hu.dwim.perec::persistent
                  hu.dwim.perec::transaction
                  hu.dwim.perec::transaction-event)
                (call-next-layered-method)))

(def layered-method collect-presented-slots :around ((component maker/abstract) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) value)
  (remove-if [and (typep !1 'hu.dwim.meta-model::effective-property)
                   (not (hu.dwim.meta-model::editable-p !1))]
             (call-next-layered-method)))
