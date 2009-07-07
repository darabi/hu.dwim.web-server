;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Title widget

(def (component e) title/widget (content/abstract)
  ()
  (:default-initargs :style-class "title")
  (:documentation "A COMPONENT that represents the title of another COMPONENT."))

(def (macro e) title ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'title/widget ,@args :content ,(the-only-element content)))

;;;;;;
;;; Title bar widget

(def (component e) title-bar/widget (style/abstract title/mixin)
  ()
  (:default-initargs :style-class "title-bar")
  (:documentation "A COMPONENT that has a title and various other small widgets around the title."))

(def render-xhtml title-bar/widget ()
  (with-render-style/abstract (-self-)
    #+nil(delegate-render -self- (class-prototype 'context-menu/mixin))
    (render-component (title-of -self-))
    #+nil(delegate-render -self- (class-prototype 'visibility/mixin))
    #+nil(delegate-render -self- (class-prototype 'collapsible/mixin))))
