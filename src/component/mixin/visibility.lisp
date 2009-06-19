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
    :computed-in compute-as
    :documentation "TRUE means the COMPONENT is visible on the remote side, FALSE otherwise."))
  (:documentation "A COMPONENT that can be HIDDEN or SHOWN."))

(def render-component :around visibility/mixin
  (when (visible-component? -self-)
    (call-next-method)))

(def method hide-component ((self visibility/mixin))
  (setf (visible-component? self) #f))

(def method show-component ((self visibility/mixin))
  (setf (visible-component? self) #t))

(def method show-component-recursively ((self visibility/mixin))
  (map-descendant-components self
                             (lambda (descendant)
                               (bind ((slot (find-slot (class-of descendant) 'visible))
                                      (slot-value (standard-instance-access descendant (slot-definition-location slot))))
                                 (when (eq #f slot-value) ; avoid making components visible with computed visible flags
                                   (show-component descendant))))))
