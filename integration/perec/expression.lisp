;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;; KLUDGE: define the perec type parameter to really be a type, so that typep checks do not fail
(def type hu.dwim.perec::a ()
  t)

(def method expression-application-type ((component expression-component) (name symbol))
  (case name
    (slot-value '(function (hu.dwim.perec::persistent-object symbol) t))
    (list '(function (&rest hu.dwim.perec::a) list))
    (t (bind ((ftype (hu.dwim.perec::persistent-ftype-of name)))
         (if (eq ftype hu.dwim.perec::+unknown-type+)
             (call-next-method)
             ftype)))))
