;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; list/widget

(def (component e) list/widget (widget/style
                                list/layout
                                selection/mixin
                                command-bar/mixin
                                context-menu/mixin
                                resizable/mixin
                                scrollable/mixin
                                collapsible/mixin
                                page-navigation-bar/mixin)
  ()
  (:documentation "A LIST/WIDGET has several COMPONENTs inside either positioned vertically or horizontally. It supports expanding, resizing, scrolling, page navigation, selection, highlighting and commands."))

(def (macro e) list/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'list/widget ,@args :contents (list ,@contents)))

(def refresh-component list/widget
  (bind (((:slots contents page-navigation-bar) -self-))
    (setf page-navigation-bar (make-instance 'page-navigation-bar/widget :total-count (length contents)))))

(def render-xhtml list/widget
  (bind (((:read-only-slots id style-class custom-style) -self-))
    <div (:id ,id :class `str("list widget" ,style-class) :style ,custom-style
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      ,(call-next-method)
      ,(render-page-navigation-bar-for -self-)>))
