;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Context menu mixin

(def (component e) context-menu/mixin ()
  ((context-menu :type component))
  (:documentation "A COMPONENT with a CONTEXT-MENU."))

(def refresh-component context-menu/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (context-menu-of -self-) (make-context-menu -self- class prototype value))))

(def layered-method make-context-menu-items ((component context-menu/mixin) class prototype value)
  nil)
