;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Top basic

(def (component e) top/basic (top/abstract style/abstract user-messages/mixin)
  ())

(def (macro e) top (() &body content)
  `(make-instance 'top/basic :content ,(the-only-element content)))

(def render-xhtml top/basic
  (with-render-style/abstract (-self-)
    (render-user-messages -self-)
    (call-next-method)))
