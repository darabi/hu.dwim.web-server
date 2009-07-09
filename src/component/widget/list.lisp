;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List widget

(def (component e) list/widget (widget/style
                                list/layout
                                context-menu/mixin
                                command-bar/mixin
                                selection/mixin
                                resizable/mixin
                                scrollable/mixin
                                collapsible/mixin
                                page-navigation-bar/mixin
                                frame-unique-id/mixin)
  ()
  (:documentation "A LIST/WIDGET has several COMPONENTs inside either positioned vertically or horizontally. It supports expanding, resizing, scrolling, page navigation, selection, highlighting and commands."))

(def (macro e) list/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'list/widget ,@args :contents (list ,@contents)))

(def render-xhtml list/widget
  (bind (((:read-only-slots id style-class custom-style) -self-))
    <div (:id ,id :class `str("list widget" ,style-class) :style ,custom-style
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      ,(call-next-method)
      #+nil
      ,(render-page-navigation-bar-for -self-)>))
