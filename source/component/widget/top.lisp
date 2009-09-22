;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; top/widget

(def (component e) top/widget (component-messages/widget target-place/widget top/abstract style/abstract menu-bar/mixin)
  ())

(def (macro e) top/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'top/widget ,@args :content ,(the-only-element content)))

(def constructor top/widget
  (setf (target-place-of -self-) (make-object-slot-place -self- (find-slot (class-of -self-) 'content))))

(def render-xhtml top/widget
  (with-render-style/abstract (-self-)
    (render-menu-bar-for -self-)
    (render-component-messages-for -self-)
    (render-content-for -self-)))
