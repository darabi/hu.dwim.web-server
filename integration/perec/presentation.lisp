;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

(def layered-method collect-slot-value-list/slots :around (component (class hu.dwim.perec::persistent-class) (prototype hu.dwim.perec::persistent-object) value)
  (remove-slots '(hu.dwim.perec::oid hu.dwim.perec::persistent hu.dwim.perec::transaction hu.dwim.perec::transaction-event) (call-next-method)))