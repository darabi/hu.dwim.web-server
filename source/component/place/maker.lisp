;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/maker

(def (component e) place/maker (t/maker place/presentation)
  ()
  (:documentation "An PLACE/MAKER makes existing values of a TYPE at a PLACE."))

(def subtype-mapper *maker-type-mapping* place place/maker)

(def layered-method make-alternatives ((component place/maker) class prototype value)
  (list (delay-alternative-component-with-initargs 'place/value/maker :component-value value)
        (delay-alternative-reference 'place/reference/maker value)))

;;;;;;
;;; place/reference/maker

(def (component e) place/reference/maker (t/reference/maker place/reference/presentation)
  ())

;;;;;;
;;; place/value/maker

(def (component e) place/value/maker (maker/basic place/value/presentation)
  ())

(def (macro e) place/value/maker (place &rest args &key &allow-other-keys)
  `(make-instance 'place/value/maker ,@args :component-value ,place))

(def layered-method make-slot-value/content ((component place/value/maker) class prototype value)
  (make-maker (place-type value) :initial-alternative-type 't/reference/maker))
