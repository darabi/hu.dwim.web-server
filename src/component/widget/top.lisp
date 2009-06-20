;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Top

(def (macro e) top ((&rest args &key &allow-other-keys) &body content)
  `(top/basic ,args ,@content))

;;;;;;
;;; Top basic

(def (component e) top/basic (top/abstract style/abstract component-messages/basic)
  ())

(def (macro e) top/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'top/basic ,@args :content ,(the-only-element content)))

(def render-xhtml top/basic
  (with-render-style/abstract (-self-)
    (render-component-messages -self-)
    (call-next-method)))
