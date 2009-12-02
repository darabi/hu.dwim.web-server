;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/inspector

(def (component e) place/inspector (t/inspector place/presentation)
  ()
  (:documentation "An PLACE/INSPECTOR displays or edits existing values of a TYPE at a PLACE."))

(def subtype-mapper *inspector-type-mapping* place place/inspector)

(def layered-method make-alternatives ((component place/inspector) class prototype value)
  (list (delay-alternative-component-with-initargs 'place/value/inspector :component-value value)
        (delay-alternative-reference 'place/reference/inspector value)))

;;;;;;
;;; place/reference/inspector

(def (component e) place/reference/inspector (t/reference/inspector place/reference/presentation)
  ())

;;;;;;
;;; place/value/inspector

(def (component e) place/value/inspector (inspector/basic place/value/presentation)
  ())

(def layered-method make-slot-value/content ((component place/value/inspector) class prototype value)
  (if (place-bound? value)
      (make-inspector (place-type value)
                      :value (value-at-place value)
                      :editable (editable-component? component)
                      :edited (edited-component? component)
                      :initial-alternative-type 't/reference/inspector)
      (make-instance 'unbound/inspector)))
