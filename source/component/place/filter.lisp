;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/filter

(def (component e) place/filter (t/filter place/presentation)
  ())

(def layered-method make-place-filter (type &rest args &key &allow-other-keys)
  (apply #'make-instance 'place/filter :component-value type args))

;;;;;;
;;; place/reference/filter

(def (component e) place/reference/filter (t/reference/filter place/reference/presentation)
  ())

;;;;;;
;;; place/value/filter

(def (component e) place/value/filter (filter/basic place/value/presentation)
  ())

(def layered-method make-slot-value/content ((component place/value/filter) class prototype value)
  (make-filter (place-type value) :initial-alternative-type 't/reference/filter))

;;;;;;
;;; Factory

(def layered-method make-place-filter (type &rest args &key &allow-other-keys)
  (apply #'make-instance 'place/filter :component-value type args))
