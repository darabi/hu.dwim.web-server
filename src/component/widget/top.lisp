;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Top widget

(def (component e) top/widget (top/abstract style/abstract component-messages/widget widget/basic)
  ())

(def (macro e) top/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'top/widget ,@args :content ,(the-only-element content)))

(def render-xhtml top/widget
  (with-render-style/abstract (-self-)
    (render-component-messages-for -self-)
    (render-content-for -self-)))
