;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Empty/layout

(eval-always
  (def (component e) empty/layout ()
    ()
    (:documentation "A completely empty (practically invisible) LAYOUT that is used as a singleton (for performance reasons) instead of NIL. The value NIL is not a valid COMPONENT for debugging purposes.")))

(def load-time-constant +empty-layout-singleton-instance+ (make-instance 'empty/layout))

(def (macro e) empty ()
  '+empty-layout-singleton-instance+)

(def render-component empty/layout
  (values))
