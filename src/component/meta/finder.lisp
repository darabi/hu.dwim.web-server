;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Finder abstract

(def (component e) finder/abstract (component-value/mixin)
  ())

;;;;;;
;;; Finder minimal

(def (component e) finder/minimal (finder/abstract component/minimal)
  ())

;;;;;;
;;; Finder basic

(def (component e) finder/basic (finder/minimal component/basic)
  ())

;;;;;;
;;; Finder style

(def (component e) finder/style (finder/basic component/style)
  ())

;;;;;;
;;; Finder full

(def (component e) finder/full (finder/style component/full)
  ())
