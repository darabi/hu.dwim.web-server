;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Empty

(eval-always
  (def (component e) empty ()
    ()
    (:documentation "A completely empty component that is used as a singleton instead of NIL. NIL is not a valid component for debugging purposes.")))

(def load-time-constant +empty-component-singleton-instance+ (make-instance 'empty-component))

(def render-component empty
  (values))

(def (macro e) empty ()
  '+empty-component-singleton-instance+)
