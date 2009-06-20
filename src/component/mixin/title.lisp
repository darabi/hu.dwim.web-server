;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Title mixin

(def (component e) title/mixin (refreshable/mixin)
  ((title :type component))
  (:documentation "A COMPONENT with a TITLE."))

(def refresh-component title/mixin
    (bind ((class (component-dispatch-class -self-))
           (prototype (component-dispatch-prototype -self-))
           (value (component-value-of -self-)))
    (setf (title-of -self-) (make-title -self- class prototype value))))

(def (layered-function e) render-title (component)
  (:method :in xhtml-layer ((self number))
    <div (:class "title")
      ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <div (:class "title")
      ,(render-component self)>)

  (:method ((self component))
    (render-component self)))

(def (layered-function e) make-title (component class prototype value)
  (:documentation "Creates the title of a component.")

  (:method ((self title/mixin) class prototype value)
    ;; don't change the title by default
    (title-of self)))

;;;;;;
;;; Title bar mixin

(def (component e) title-bar/mixin ()
  ((title-bar :type component))
  (:documentation "A COMPONENT with a TITLE-BAR."))
