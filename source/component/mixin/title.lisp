;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; title/mixin

(def (component e) title/mixin (refreshable/mixin)
  ((title nil :type component))
  (:documentation "A COMPONENT with a TITLE."))

(def refresh-component title/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (title-of -self-) (make-title -self- class prototype value))))

(def (function e) render-title-for (component)
  (awhen (title-of component)
    (render-title it)))

(def (layered-function e) render-title (component)
  (:method :in xhtml-layer ((self number))
    <span (:class "title")
      ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <span (:class "title")
      ,(render-component self)>)

  (:method ((self component))
    (render-component self))

  (:method ((self string))
    (render-component self))

  (:method ((self number))
    (render-component self)))

(def layered-method make-title ((self title/mixin) class prototype value)
  ;; don't change the title by default
  (title-of self))

;;;;;;
;;; title-bar/mixin

(def (component e) title-bar/mixin ()
  ((title-bar :type component))
  (:documentation "A COMPONENT with a TITLE-BAR."))

(def (function e) render-title-bar-for (component)
  (render-component (title-bar-of component)))
