;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Renderable mixin

(def (component e) renderable/mixin ()
  ((to-be-rendered
    #t
    :type boolean
    :documentation "TRUE means the COMPONENT must be rendered to the remote side to update its content, FALSE otherwise. The flag is automatically cleared upon each RENDER."))
  (:documentation "A COMPONENT that supports the ajax RENDER protocol. The COMPONENT will be rendered to the remote side if the flag is set to TRUE, otherwise it is unspecified when RENDER will be called."))

(def render-xhtml :after renderable/mixin
  (mark-rendered -self-))

(def method mark-to-be-rendered ((self renderable/mixin))
  (setf (to-be-rendered? self) #t))

(def method mark-rendered ((self renderable/mixin))
  (setf (to-be-rendered? self) #f))

(def function to-be-rendered-slot? (slot)
  (eq 'to-be-rendered (slot-definition-name slot)))

(def method (setf slot-value-using-class) (new-value (class component-class) (instance renderable/mixin) (slot standard-effective-slot-definition))
  (unless (or (to-be-rendered-slot? slot)
              (eq (standard-instance-access instance (slot-definition-location slot)) new-value))
    (call-next-method)
    (mark-to-be-rendered instance)))

(def method slot-makunbound-using-class ((class component-class) (instance renderable/mixin) (slot standard-effective-slot-definition))
  (unless (or (to-be-rendered-slot? slot)
              (slot-boundp-using-class class instance slot))
    (call-next-method)
    (mark-to-be-rendered instance)))
