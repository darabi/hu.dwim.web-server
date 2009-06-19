;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Empty/layout

(eval-always
  (def (component e) empty/layout ()
    ()
    (:documentation "A completely EMPTY (practically invisible) COMPONENT that is used as a singleton (for performance reasons) instead of NIL. The value NIL is not a valid COMPONENT for debugging purposes.")))

(def load-time-constant +empty-layout-singleton-instance+ (make-instance 'empty/layout))

(def render-component empty/layout
  (values))

(def (macro e) empty ()
  '+empty-layout-singleton-instance+)
