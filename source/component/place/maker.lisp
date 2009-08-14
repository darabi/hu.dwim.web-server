;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Place maker

(def (component e) place-maker (place-component maker/abstract)
  ((name nil)
   (the-type nil)
   (initform)))

(def method make-place-component-content ((self place-maker))
  (make-maker (the-type-of self)))
