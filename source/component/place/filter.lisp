;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/filter

(def (component e) place/filter (t/filter)
  ()
  (:documentation "A PLACE/INSPECTOR filters existing values of a TYPE at a PLACE."))

;;;;;;
;;; place/alternator/filter

(def (component e) place/alternator/filter (t/alternator/filter place/presentation)
  ())

(def subtype-mapper *filter-type-mapping* place place/alternator/filter)

(def layered-method make-alternatives ((component place/alternator/filter) class prototype value)
  (list (make-instance 'place/value/filter :component-value value)
        (make-instance 'place/reference/filter :component-value value)))

;;;;;;
;;; place/reference/filter

(def (component e) place/reference/filter (t/reference/filter place/reference/presentation)
  ())

;;;;;;
;;; place/value/filter

(def (component e) place/value/filter (t/detail/filter place/value/presentation)
  ())

(def layered-method make-content-presentation ((component place/value/filter) class prototype value)
  (make-filter (place-type value) :initial-alternative-type 't/reference/filter))
