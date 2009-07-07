;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu bar mixin

(def (component e) menu-bar/mixin ()
  ((menu-bar :type component :documentation "A NIL value specifies no MENU-BAR."))
  (:documentation "A COMPONENT with a MENU-BAR."))

(def refresh-component menu-bar/mixin
  (unless (slot-boundp -self- 'menu-bar)
    (bind ((class (component-dispatch-class -self-))
           (prototype (component-dispatch-prototype -self-))
           (value (component-value-of -self-)))
      (setf (menu-bar-of -self-) (make-menu-bar -self- class prototype value)))))

(def (function e) render-menu-bar-for (component)
  (awhen (menu-bar-of component)
    (render-component it)))

(def layered-method make-menu-bar ((component menu-bar/mixin) class prototype value)
  (make-instance 'menu-bar/widget :menu-items (make-menu-bar-items component class prototype value)))
