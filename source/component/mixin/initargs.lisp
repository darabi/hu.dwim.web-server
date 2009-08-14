;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; initargs/mixin

(def (component e) initargs/mixin ()
  ((initargs :type list :documentation "The list of captured initargs."))
  (:documentation "A COMPONENT that captures the initargs as they were originally provided when the COMPONENT was last time initialized."))

(def method shared-initialize :after ((self initargs/mixin) slot-names &rest initargs &key &allow-other-keys)
  (setf (initargs-of self)
        (iter (for arg :in (collect-captured-initargs self))
              (for value = (getf initargs arg :unbound))
              (unless (eq value :unbound)
                (collect arg)
                (collect value)))))

(def (layered-function e) collect-captured-initargs (component)
  (:method ((self initargs/mixin))
    nil))

(def (function e) inherited-initarg (component key)
  (awhen (find-ancestor-component-with-type component 'initargs/mixin)
    (bind ((value (getf (initargs-of it) key :unbound)))
      (if (eq value :unbound)
          (values nil #f)
          (values value #t)))))
