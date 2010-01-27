;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; refreshable/mixin

(def (component e) refreshable/mixin ()
  ((to-be-refreshed-component
    #t
    :type boolean
    :documentation "TRUE means COMPONENT will be refreshed before the next RENDER-COMPONENT, FALSE otherwise."))
  (:documentation "A COMPONENT that supports the REFRESH-COMPONENT protocol. Refresh is used to update the component's state based on some other data specific to the COMPONENT."))

(def render-component :before refreshable/mixin
  ;; we put it in a :before so that more specialized :before's can happend before this one
  (ensure-refreshed -self-))

(def layered-method refresh-component :after ((self refreshable/mixin))
  (mark-refreshed-component self))

(def layered-method make-command-bar-commands ((component refreshable/mixin) class prototype value)
  (optional-list* (make-refresh-component-command component class prototype value) (call-next-method)))

(def layered-method make-context-menu-items ((component refreshable/mixin) class prototype value)
  (optional-list* (make-menu-item (make-refresh-component-command component class prototype value)) (call-next-method)))

(def method mark-to-be-refreshed-component ((self refreshable/mixin))
  (setf (to-be-refreshed-component? self) #t))

(def method mark-refreshed-component ((self refreshable/mixin))
  (setf (to-be-refreshed-component? self) #f))

(def function ensure-refreshed (component)
  (when (to-be-refreshed-component? component)
    (refresh-component component))
  component)

(def method (setf component-value-of) :after (new-value (self refreshable/mixin))
  (mark-to-be-refreshed-component self))
