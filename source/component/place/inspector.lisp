;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/inspector

(def (component e) place/inspector (t/inspector place/presentation)
  ())

(def layered-method make-alternatives ((component place/inspector) class prototype value)
  (list (delay-alternative-reference 'place/reference/inspector value)
        (delay-alternative-component-with-initargs 'place/value/inspector :component-value value)))

;;;;;;
;;; place/reference/inspector

(def (component e) place/reference/inspector (t/reference/inspector place/reference/presentation)
  ())

;;;;;;
;;; place/value/inspector

(def (component e) place/value/inspector (inspector/basic place/value/presentation)
  ())

(def layered-method make-slot-value/content ((component place/value/inspector) class prototype value)
  (make-inspector (place-type value)
                  ;; TODO: handle unbound in a better way
                  (if (place-bound? value)
                      (value-at-place value)
                      (make-instance 'unbound/inspector))))

;;;;;;
;;; Factory

(def layered-method make-place-inspector (type &rest args &key &allow-other-keys)
  (apply #'make-instance 'place/inspector :component-value type args))