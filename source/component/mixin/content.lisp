;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; content/mixin

(def (component e) content/mixin ()
  ((content
    nil
    :type (or null component)
    :documentation "The content is a single COMPONENT."))
  (:documentation "A COMPONENT that has another COMPONENT inside."))

(def (function e) render-content-for (component)
  (render-component (content-of component)))

;;;;;;
;;; content/component

(def (component e) content/component (refreshable/mixin content/mixin)
  ())

(def refresh-component content/component
  (awhen (content-of -self-)
    (mark-to-be-refreshed-component it)))

(def method component-value-of ((self content/component))
  (awhen (content-of self)
    (component-value-of it)))

(def method (setf component-value-of) (new-value (self content/component))
  (awhen (content-of self)
    (setf (component-value-of it) new-value)))

;;;;;;
;;; contents/mixin

(def (component e) contents/mixin ()
  ((contents
    nil
    :type components
    :documentation "The content is a sequence of COMPONENTs."))
  (:documentation "A COMPONENT that has a set of COMPONENTs inside."))

(def (function e) render-contents-for (component)
  (foreach #'render-component (contents-of component)))

;;;;;;
;;; contents/component

(def (component e) contents/component (refreshable/mixin contents/mixin)
  ())

(def refresh-component contents/component
  (foreach #'mark-to-be-refreshed-component (contents-of -self-)))

(def method component-value-of ((self contents/component))
  (mapcar 'component-value-of (contents-of self)))

(def method (setf component-value-of) (new-value (self contents/component))
  (mapcar [setf (component-value-of !1) !2] (contents-of self) new-value))
