;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/maker

(def (component e) component/maker (component/presentation)
  ()
  (:documentation "A maker creates new values of a TYPE. This class does not have any slots on purpose.
  - similar to (make-instance ...)
  - static input
    - value-type: type
  - volatile input
    - selected-type: type (selected-type is a subtype of value-type)
    - value: selected-type
    - restrictions: (e.g. initargs)
  - output
    - value: selected-type"))

;;;;;;
;;; t/maker

(def (component e) t/maker (component/maker t/presentation)
  ())

(def method component-dispatch-class ((self t/maker))
  (or (find-class-for-type (component-value-type-of self))
      (find-class t)))

;;;;;;
;;; Maker factory

(def (special-variable e) *maker-type-mapping* (make-linear-type-mapping))

(def subtype-mapper *maker-type-mapping* nil nil)

(def layered-method make-maker (type &rest args &key value &allow-other-keys)
  (bind ((class (find-class (linear-mapping-value *maker-type-mapping* type))))
    (apply #'make-instance class
           :component-value value
           :component-value-type type
           (remove-undefined-class-slot-initargs class args))))
