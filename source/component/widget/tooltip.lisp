;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; tooltip/widget

(def (component e) tooltip/widget (content/mixin)
  ()
  (:documentation "A COMPONENT which pops up as a tooltip of another COMPONENT."))

(def (macro e) tooltip/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tooltip/widget ,@args :content ,(the-only-element content)))

(def render-component tooltip/widget
  <div ,@(with-xhtml-body-environment ()
           (with-active-layers (passive-xhtml-layer)
             (render-content-for -self-)))>)
