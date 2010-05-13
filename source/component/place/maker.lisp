;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; place/maker

(def (component e) place/maker (t/maker)
  ()
  (:documentation "An PLACE/MAKER makes existing values of a TYPE at a PLACE."))

;;;;;;
;;; place/alternator/maker

(def (component e) place/alternator/maker (t/maker place/presentation)
  ())

(def subtype-mapper *maker-type-mapping* place place/alternator/maker)

(def layered-method make-alternatives ((component place/alternator/maker) class prototype value)
  (list (make-instance 'place/value/maker :component-value value)
        (make-instance 'place/reference/maker :component-value value)))

;;;;;;
;;; place/reference/maker

(def (component e) place/reference/maker (t/reference/maker place/reference/presentation)
  ())

;;;;;;
;;; place/value/maker

(def (component e) place/value/maker (t/detail/maker place/value/presentation)
  ())

(def (macro e) place/value/maker (place &rest args &key &allow-other-keys)
  `(make-instance 'place/value/maker ,@args :component-value ,place))

(def layered-method make-content-presentation ((component place/value/maker) class prototype value)
  (make-maker (place-type value) :initial-alternative-type 't/reference/maker))
