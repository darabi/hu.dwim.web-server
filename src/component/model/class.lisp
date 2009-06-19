;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard class mixin

(def (component e) standard-class/mixin ()
  ((the-class
    nil
    :type (or null standard-class)
    :computed-in compute-as))
  (:documentation "A component with a STANDARD-CLASS."))

;;;;;;
;;; Standard class abstract

(def (component e) standard-class/abstract (dispatch-class/abstract standard-class/mixin)
  ()
  (:documentation "A component with a STANDARD-CLASS component value."))

(def method component-value-of ((component standard-class/abstract))
  (the-class-of component))

(def method (setf component-value-of) (new-value (component standard-class/abstract))
  (setf (the-class-of component) new-value))

(def method component-dispatch-class ((self standard-class/abstract))
  (the-class-of self))

;;;;;;
;;; Standard class inspector

(def (component e) standard-class/inspector (standard-class/mixin alternator/basic)
  ()
  (:documentation "Component for an instance of STANDARD-CLASS in various alternative views"))

(def layered-method make-alternatives ((component standard-class/inspector) (class standard-class) (prototype standard-object) (instance standard-object))
  (list (delay-alternative-component-with-initargs 'standard-class/detail/inspector :the-class class)
        (delay-alternative-reference-component 'standard-class-reference class)))

;;;;;;
;;; Standard class detail inspector

(def (component e) standard-class/detail/inspector (detail/abstract standard-class/mixin)
  ((metaclass nil :type component)
   (direct-subclasses nil :type component)
   (direct-superclasses nil :type component)
   (direct-slots nil :type component)
   (effective-slots nil :type component))
  (:documentation "Component for an instance of STANDARD-CLASS in detail"))

(def refresh-component standard-class/detail/inspector
  (bind (((:slots the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) -self-))
    (if the-class
        (progn
          (if metaclass
              (setf (component-value-of metaclass) (class-of the-class))
              (setf metaclass (make-viewer (class-of the-class) :initial-alternative-type 'reference-component)))
          (if direct-subclasses
              (setf (component-value-of direct-subclasses) (class-direct-subclasses the-class))
              (setf direct-subclasses (make-instance 'reference-list-component :targets (class-direct-subclasses the-class))) )
          (if direct-superclasses
              (setf (component-value-of direct-superclasses) (class-direct-superclasses the-class))
              (setf direct-superclasses (make-instance 'reference-list-component :targets (class-direct-superclasses the-class))))
          (if direct-slots
              (setf (component-value-of direct-slots) (class-direct-slots the-class))
              (setf direct-slots (make-instance 'standard-slot-definition/table/inspector :slots (class-direct-slots the-class))))
          (if effective-slots
              (setf (component-value-of effective-slots) (class-slots the-class))
              (setf effective-slots (make-instance 'standard-slot-definition/table/inspector :slots (class-slots the-class)))))
        (setf metaclass nil
              direct-subclasses nil
              direct-superclasses nil
              direct-slots nil
              effective-slots nil))))

(def render-xhtml standard-class/detail/inspector
  (bind (((:read-only-slots the-class metaclass direct-subclasses direct-superclasses direct-slots effective-slots) -self-)
         (class-name (qualified-symbol-name (class-name the-class))))
    <div <span "The class " <i ,class-name> " is an instance of " ,(render-component metaclass)>
         <div <h3 "Direct super classes">
              ,(render-component direct-superclasses)>
         <div <h3 "Direct sub classes">
              ,(render-component direct-subclasses)>
         <div <h3 "Direct slots">
              ,(render-component direct-slots)>
         <div <h3 "Effective slots">
              ,(render-component effective-slots)>>))
