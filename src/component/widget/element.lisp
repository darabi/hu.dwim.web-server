;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Element widget

(def (component e) element/widget (widget/basic
                                   context-menu/mixin
                                   content/abstract
                                   selectable/mixin
                                   frame-unique-id/mixin)
  ()
  (:documentation "TODO: expandible"))

(def (macro e) element/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'element/widget ,@args :content ,(the-only-element content)))

(def render-xhtml element/widget
  (bind (((:read-only-slots id) -self-))
    <div (:id ,id :class ,(concatenate-string "element widget " (selectable-component-style-class -self-)))
      ,(render-context-menu-for -self-)
      ,(render-content-for -self-)>
    (render-command-onclick-handler (find-command -self- 'select-component) id)))

;; TODO: move this to element/editor
(def layered-method make-context-menu-items ((component element/widget) class prototype value)
  (optional-list* (make-menu-item (make-remove-list-element-command component class prototype value) nil)
                  (make-menu-item (make-select-component-command component class prototype value) nil)
                  (call-next-method)))

(def (icon e) remove-list-element)

(def (layered-function e) make-remove-list-element-command (component class prototype value)
  (:method ((component element/widget) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon remove-list-element)
      (make-component-action component
        (appendf (contents-of component) (list (remove-list-element component class prototype value)))))))

(def (generic e) remove-list-element (component class prototype value))

;;;;;;
;;; Element viewer

(def (component e) element/viewer (viewer/basic element/widget)
  ())

;;;;;;
;;; Element editor

(def (component e) element/editor (editor/basic element/widget)
  ())

;;;;;;
;;; Element inspector

(def (component e) element/inspector (inspector/basic element/widget)
  ())
