;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; top/widget

(def (component e) top/widget (component-messages/widget
                               target-place/widget
                               top/component
                               menu-bar/mixin
                               context-menu/mixin)
  ())

(def (macro e) top/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'top/widget ,@args :content ,(the-only-element content)))

(def render-xhtml top/widget
  (with-render-style/component (-self-)
    (render-menu-bar-for -self-)
    (render-context-menu-for -self-)
    (render-component-messages-for -self-)
    (render-content-for -self-)))
