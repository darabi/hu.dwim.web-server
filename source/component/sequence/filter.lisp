;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; sequence/filter

(def (component e) sequence/filter (t/filter)
  ())

(def method collect-filter-predicates ((self sequence/filter))
  '(some every))
