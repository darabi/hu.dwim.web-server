;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Initargs mixin

(def (component ea) initargs/mixin ()
  ((initargs :type list))
  (:documentation "A component that captures the initargs as they were originally provided when the component was last time reinitialized."))

(def method shared-initialize :after ((self initargs/mixin) slot-names &rest initargs &key &allow-other-keys)
  (setf (initargs-of -self-)
        (iter (for arg :in (collect-captured-initargs self))
              (for value = (getf args arg :unbound))
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
