;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;; KLUDGE: define the perec type parameter to really be a type, so that typep checks do not fail
(def type cl-perec::a ()
  t)

(def method expression-application-type ((component expression-component) (name symbol))
  (case name
    (slot-value '(function (cl-perec::persistent-object symbol) t))
    (list '(function (&rest cl-perec::a) list))
    (t (bind ((ftype (cl-perec::persistent-ftype-of name)))
         (if (eq ftype cl-perec::+unknown-type+)
             (call-next-method)
             ftype)))))
