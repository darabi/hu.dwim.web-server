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

(def render-xhtml list/widget
  (bind (((:read-only-slots id style-class custom-style orientation contents page-navigation-bar) -self-))
    <div (:id ,id :class `str("list widget " ,style-class) :style ,custom-style
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      ,(render-list-layout orientation (make-page-navigation-contents page-navigation-bar contents))
      ,(render-page-navigation-bar-for -self-)>))

(def layered-method make-page-navigation-bar ((component list/widget) class prototype value)
  (make-instance 'page-navigation-bar/widget :total-count (length (contents-of component))))

(def method map-visible-child-components ((component list/widget) function)
  (bind ((visitor (lambda (child)
                    (when (visible-component? child)
                      (funcall function child)))))
    (map-child-components component visitor
                          (lambda (component)
                            (remove-slots '(contents) (visible-child-component-slots component))))
    (foreach visitor (make-page-navigation-contents (page-navigation-bar-of component) (contents-of component)))))
