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
  (unless (slot-boundp -self- 'context-menu)
    (bind ((class (component-dispatch-class -self-))
           (prototype (component-dispatch-prototype -self-))
           (value (component-value-of -self-)))
      (setf (context-menu-of -self-) (make-context-menu -self- class prototype value)))))

(def (function e) render-context-menu-for (component)
  (awhen (context-menu-of component)
    (render-component it)))

(def layered-method make-context-menu ((component context-menu/mixin) class prototype value)
  (make-instance 'context-menu/widget
                 :target component
                 :menu-items (make-context-menu-items component class prototype value)))
