;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; element/widget

(def (component e) element/widget (standard/widget
                                   context-menu/mixin
                                   content/component
                                   selectable/mixin)
  ()
  (:documentation "An ELEMENT/WIDGET has a single COMPONENT inside. It supports selection, highlight and commands within a LIST/WIDGET."))

(def (macro e) element/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'element/widget ,@args :content ,(the-only-element content)))

(def render-xhtml element/widget
  (bind (((:read-only-slots id style-class custom-style) -self-))
    <div (:id ,id :class `str("element widget " ,style-class ,(selectable-component-style-class -self-)) :style ,custom-style
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      ,(render-content-for -self-)>
    (when-bind select-command (find-command -self- 'select-component)
      (render-command-onclick-handler select-command id))))

(def (function e) element-style-class (index total)
  (string+ (when (zerop index)
             "first ")
           (when (= total (1+ index))
             "last ")
           (if (zerop (mod index 2))
               "even"
               "odd")))
