;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Title basic

(def (component ea) title/basic (content/basic)
  ()
  (:default-initargs :style-class "title")
  (:documentation "A component that represents the title of another component."))

(def (macro e) title ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'title/basic ,@args :content ,(the-only-element content)))

;;;;;;
;;; Title mixin

(def (component ea) title/mixin ()
  ((title :type component*))
  (:documentation "A component with a title."))

(def refresh title/mixin
  (setf (title-of -self-) (make-title -self-)))

(def (layered-function e) make-title (component)
  (:documentation "Creates the title of a component.")

  (:method ((self title/mixin))
    ;; don't change the title by default
    (title-of self)))

;;;;;;
;;; Title bar mixin

(def (component ea) title-bar/mixin ()
  ((title-bar :type component*))
  (:documentation "A component with a title bar."))

;;;;;;
;;; Title bar basic

(def (component ea) title-bar/basic (style/abstract title/mixin)
  ()
  (:default-initargs :style-class "title-bar")
  (:documentation "A component that has a title and various other small widgets around the title."))

(def render-xhtml title-bar/basic ()
  (with-render-style/abstract (-self-)
    (delegate-render -self- (class-prototype 'context-menu/mixin))
    (render-component (title-of -self-))
    (delegate-render -self- (class-prototype 'visible/mixin))
    (delegate-render -self- (class-prototype 'expandible/mixin))))
