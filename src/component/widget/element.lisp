;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Element widget

(def (component e) element/widget (widget/style
                                   context-menu/mixin
                                   content/abstract
                                   selectable/mixin
                                   frame-unique-id/mixin)
  ()
  (:documentation "An ELEMENT/WIDGET has a single COMPONENT inside. It supports selection, highlight and commands."))

(def (macro e) element/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'element/widget ,@args :content ,(the-only-element content)))

(def render-xhtml element/widget
  (bind (((:read-only-slots id style-class custom-style) -self-))
    <div (:id ,id :class `str("element widget " ,style-class ,(selectable-component-style-class -self-)) :style ,custom-style
          :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
          :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      ,(render-context-menu-for -self-)
      ,(render-content-for -self-)>
    (render-command-onclick-handler (find-command -self- 'select-component) id)))

(def layered-method make-context-menu-items ((component element/widget) class prototype value)
  (optional-list* (make-menu-item (make-select-component-command component class prototype value) nil)
                  (call-next-method)))
