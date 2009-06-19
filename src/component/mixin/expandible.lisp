;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Expandible mixin

(def (component e) expandible/mixin ()
  ((expanded-component
    #t
    :type boolean
    :computed-in compute-as
    :documentation "TRUE means COMPONENT displays itself with full detail, FALSE means it should be minimized."))
  (:documentation "A COMPONENT that can be EXPANDED or COLLAPSED."))

(def method expand-component ((self expandible/mixin))
  (setf (expanded-component? self) #t))

(def method collapse-component ((self expandible/mixin))
  (setf (expanded-component? self) #f))
