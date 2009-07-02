;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Visibility mixin

(def (component e) visibility/mixin ()
  ((visible-component
    #t
    :type boolean
    :initarg :visible
    :computed-in compute-as
    :documentation "TRUE means the COMPONENT is visible on the remote side, FALSE otherwise."))
  (:documentation "A COMPONENT that can be HIDDEN or SHOWN."))

(def render-component :around visibility/mixin
  (when (force (visible-component? -self-))
    (call-next-method)))

(def method hide-component ((self visibility/mixin))
  (setf (visible-component? self) #f))

(def method show-component ((self visibility/mixin))
  (setf (visible-component? self) #t))
