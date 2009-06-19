;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Refreshable mixin

(def (icon e) refresh)

(def (component e) refreshable/mixin ()
  ((to-be-refreshed
    #t
    :type boolean
    :documentation "TRUE means the component must be refreshed before the executing the next render, FALSE otherwise."))
  (:documentation "A COMPONENT that supports the REFRESH-COMPONENT protocol. Refresh is used to update the component's state based on some other data."))

(def render-component :before refreshable/mixin
  ;; we put it in a :before so that more specialized :before's can happend before it
  (ensure-refreshed -self-))

(def layered-method refresh-component :after ((self refreshable/mixin))
  (mark-component-refreshed self))

(def layered-method make-refresh-command ((component refreshable/mixin) class prototype value)
  (command ()
    (icon refresh)
    (make-component-action component
      (refresh-component component))))

(def layered-method make-command-bar-commands ((component refreshable/mixin) class prototype value)
  (optional-list* (make-refresh-command component class prototype value) (call-next-method)))

(def layered-method make-context-menu-items ((component refreshable/mixin) class prototype value)
  (optional-list* (make-refresh-command component class prototype value) (call-next-method)))

(def method mark-component-to-be-refreshed ((self refreshable/mixin))
  (setf (to-be-refreshed? self) #t))

(def method mark-component-refreshed ((self refreshable/mixin))
  (setf (to-be-refreshed? self) #f))

(def function ensure-refreshed (component)
  (when (or (to-be-refreshed? component)
            (some (lambda (slot)
                    (not (computed-slot-valid-p component slot)))
                  (computed-slots-of (class-of component))))
    (refresh-component component))
  component)

(def method call-compute-as :after ((self refreshable/mixin) thunk)
  (mark-component-to-be-refreshed self))

(def method (setf component-value-of) :after (new-value (self refreshable/mixin))
  (mark-component-to-be-refreshed component))
