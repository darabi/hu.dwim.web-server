;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method reuse-standard-object-instance ((class hu.dwim.perec::persistent-class) (instance hu.dwim.perec::persistent-object))
  (if (hu.dwim.perec::persistent-p instance)
      (hu.dwim.perec::load-instance instance)
      (call-next-method)))

(def layered-method make-expand-command :around ((component inspector/abstract) (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) (instance hu.dwim.perec::persistent-object))
  (when (hu.dwim.meta-model::authorize-operation 'expand-instance-operation :-entity- class)
    (call-next-method)))

(def layered-method collect-standard-object-detail-slot-groups ((component standard-object-detail-component) (class hu.dwim.meta-model::entity) (prototype hu.dwim.perec::persistent-object) (slots list))
  (bind ((slot-groups (partition slots #'hu.dwim.meta-model::primary-p (constantly #t))))
    (list (cons #"standard-object-detail-component.primary-group" (first slot-groups))
          (cons #"standard-object-detail-component.secondary-group" (second slot-groups)))))
