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

(def layered-method make-alternatives ((component t/maker) class prototype value)
  (list (delay-alternative-reference 't/reference/maker value)
        (delay-alternative-component-with-initargs 't/name-value-list/maker :component-value value)))

;;;;;;
;;; t/reference/maker

(def (component e) t/reference/maker (maker/basic t/reference/presentation)
  ())
