;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tooltip widget

(def (component e) tooltip/widget (content/mixin)
  ()
  (:documentation "A COMPONENT which pops up as a tooltip of another COMPONENT."))

(def (macro e) tooltip/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'tooltip/widget ,@args :content ,(the-only-element content)))

(def render-component tooltip/widget
  <div ,@(with-collapsed-js-scripts
          (with-dojo-widget-collector
            (with-active-layers (passive-xhtml-layer)
              (render-content-for -self-))))>)
