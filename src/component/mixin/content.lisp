;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content mixin

(def (component e) content/mixin ()
  ((content
    :type component
    :documentation "The content of is single COMPONENT."))
  (:documentation "A COMPONENT that has another COMPONENT inside."))

(def render-component content/mixin
  (render-content -self-))

(def (function e) render-content (component)
  (render-component (content-of component)))

;;;;;;
;;; Content abstract

(def (component e) content/abstract (refreshable/mixin content/mixin)
  ())

(def refresh-component content/abstract
  (awhen (content-of -self-)
    (mark-to-be-refreshed-component it)))

(def method component-value-of ((self content/abstract))
  (component-value-of (content-of self)))

(def method (setf component-value-of) (new-value (self content/abstract))
  (setf (component-value-of (content-of self)) new-value))

;;;;;;
;;; Contents mixin

(def (component e) contents/mixin ()
  ((contents
    :type components
    :documentation "The content is a sequence of COMPONENTs."))
  (:documentation "A COMPONENT that has a set of COMPONENTs inside."))

(def render-component contents/mixin
  (render-contents -self-))

(def (function e) render-contents (component)
  (foreach #'render-component (contents-of component)))

;;;;;;
;;; Contents abstract

(def (component e) contents/abstract (refreshable/mixin contents/mixin)
  ())

(def refresh-component contents/abstract
  (foreach #'mark-to-be-refreshed-component (contents-of -self-)))

(def method component-value-of ((self contents/abstract))
  (mapcar 'component-value-of (contents-of self)))

(def method (setf component-value-of) (new-value (self contents/abstract))
  (mapcar [setf (component-value-of !1) !2] (contents-of self) new-value))
