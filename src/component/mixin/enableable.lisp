;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Enableable mixin

(def (component e) enableable/mixin ()
  ((enabled-component
    #t
    :type boolean
    :initarg :enabled
    :computed-in compute-as
    :documentation "TRUE means COMPONENT is ENABLED on the remote side, FALSE otherwise."))
  (:documentation "A COMPONENT that can be ENABLED or DISABLED."))

(def method enable-component ((self enableable/mixin))
  (setf (enabled-component? self) #t))

(def method disable-component ((self enableable/mixin))
  (setf (enabled-component? self) #f))
