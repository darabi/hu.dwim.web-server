;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; t/maker

(def (component e) t/maker (maker/basic t/presentation)
  ())

(def subtype-mapper *maker-type-mapping* t t/maker)

(def layered-method make-alternatives ((component t/maker) class prototype value)
  (list (delay-alternative-component-with-initargs 't/name-value-list/maker :component-value value)
        (delay-alternative-reference 't/reference/maker value)))

;;;;;;
;;; t/reference/maker

(def (component e) t/reference/maker (maker/basic t/reference/presentation)
  ())
