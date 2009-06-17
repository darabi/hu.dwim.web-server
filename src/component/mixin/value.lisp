;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component value basic

(def (component ea) component-value/basic (cloneable/mixin)
  ())

(def layered-method clone-component :around ((self component-value/basic))
  ;; this must be done at the very last, after all primary method customization
  (prog1-bind clone (call-next-method)
    (setf (component-value-of clone) (component-value-of self))))

;;;;;;
;;; Component value mixin

(def (component ea) component-value/mixin ()
  ((component-value
    :type t
    :computed-in compute-as
    :documentation "The current value displayed by this component."))
  (:documentation "A component that displays a single value."))

(def method component-value-of ((component component-value/mixin))
  (slot-value component 'component-value))

(def method (setf component-value-of) (new-value (component component-value/mixin))
  (setf (slot-value component 'component-value) new-value))
