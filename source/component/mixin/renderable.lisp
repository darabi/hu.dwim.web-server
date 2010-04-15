;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; renderable/mixin

(def (component e) renderable/mixin ()
  ((to-be-rendered-component
    #t
    :type boolean
    :documentation "TRUE means COMPONENT will be rendered to the remote side to update its content, FALSE otherwise. The flag is automatically cleared upon each RENDER-COMPONENT invocation.")
   (rendered-component
    #f
    :type boolean
    :documentation "TRUE means COMPONENT has been rendered last time the ajax RENDER-COMPONENT protocol was run."))
  (:documentation "A COMPONENT that supports the ajax RENDER-COMPONENT protocol. The COMPONENT will be rendered to the remote side if the flag is set to TRUE, otherwise it is unspecified when RENDER-COMPONENT will be called."))

(def render-xhtml :after renderable/mixin
  (mark-rendered-component -self-))

(def method mark-to-be-rendered-component ((self renderable/mixin))
  (setf (to-be-rendered-component? self) #t))

(def method mark-rendered-component ((self renderable/mixin))
  (setf (to-be-rendered-component? self) #f)
  (setf (rendered-component? self) #t))

;; TODO: move
(def method rendered-component? ((self component))
  #t)

;; TODO: move
(def method (setf rendered-component?) (new-value (self component))
  (values))

(def function to-be-rendered-component-slot? (slot)
  (member (slot-definition-name slot) '(to-be-rendered-component rendered-component)))

(def method (setf slot-value-using-class) (new-value (class component-class) (instance renderable/mixin) (slot standard-effective-slot-definition))
  (unless (eql (standard-instance-access instance (slot-definition-location slot)) new-value)
    (call-next-method)
    (unless (or (to-be-rendered-component-slot? slot)
                (and (slot-boundp instance 'to-be-rendered-component)
                     (to-be-rendered-component? instance)))
      (mark-to-be-rendered-component instance))))

(def method slot-makunbound-using-class ((class component-class) (instance renderable/mixin) (slot standard-effective-slot-definition))
  (unless (slot-boundp-using-class class instance slot)
    (call-next-method)
    (unless (or (to-be-rendered-component-slot? slot)
                (and (slot-boundp instance 'to-be-rendered-component)
                     (to-be-rendered-component? instance)))
      (mark-to-be-rendered-component instance))))
