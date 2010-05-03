;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; column/widget

(def (component e) column/widget (widget/style
                                  column/abstract
                                  header/mixin
                                  context-menu/mixin
                                  selectable/mixin)
  ())

(def (macro e) column/widget ((&rest args &key &allow-other-keys) &body header)
  `(make-instance 'column/widget ,@args :header ,(the-only-element header)))

(def method component-style-class ((self column/widget))
  (string+ "table-header-border " (call-next-method)))

(def render-xhtml column/widget
  ;; NOTE: don't put style and the like on th, because that cannot be easily updated on the client side
  <th ,(with-render-style/abstract (-self-)
         (render-header-for -self-))>)
