;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Enabled mixin

(def (component ea) enabled/mixin ()
  ((enabled
    #t
    :type boolean
    :computed-in compute-as
    :documentation "True means the component must be enabled on the remote side, false means the opposite."))
  (:documentation "A component that can be enabled or disabled."))

(def method enable-component ((self enabled/mixin))
  (setf (enabled? self) #t))

(def method disable-component ((self enabled/mixin))
  (setf (enabled? self) #f))
