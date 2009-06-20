;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Title basic

(def (component e) title/basic (content/basic)
  ()
  (:default-initargs :style-class "title")
  (:documentation "A COMPONENT that represents the title of another COMPONENT."))

(def (macro e) title ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'title/basic ,@args :content ,(the-only-element content)))

;;;;;;
;;; Title bar basic

(def (component e) title-bar/basic (style/abstract title/mixin)
  ()
  (:default-initargs :style-class "title-bar")
  (:documentation "A component that has a title and various other small widgets around the title."))

(def render-xhtml title-bar/basic ()
  (with-render-style/abstract (-self-)
    (delegate-render -self- (class-prototype 'context-menu/mixin))
    (render-component (title-of -self-))
    (delegate-render -self- (class-prototype 'visibility/mixin))
    (delegate-render -self- (class-prototype 'expandible/mixin))))
