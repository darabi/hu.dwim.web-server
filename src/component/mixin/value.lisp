;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Component value mixin

(def (component e) component-value/mixin ()
  ((component-value
    :type t
    :computed-in compute-as
    :documentation "The current COMPONENT-VALUE that is handled by this COMPONENT."))
  (:documentation "A COMPONENT that handles a single COMPONENT-VALUE."))

(def method component-value-of ((self component-value/mixin))
  (slot-value self 'component-value))

(def method (setf component-value-of) (new-value (self component-value/mixin))
  (setf (slot-value self 'component-value) new-value))
