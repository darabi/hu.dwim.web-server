;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List widget

(def (component e) list/widget (widget/basic
                                list/layout
                                context-menu/mixin
                                command-bar/mixin
                                selection/mixin
                                page-navigation-bar/mixin
                                frame-unique-id/mixin)
  ()
  (:documentation "A LIST/WIDGET with several COMPONENTs inside. TODO: expandible, resizable, page navigation, scrolling and selection"))

(def (macro e) list/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'list/widget ,@args :contents (list ,@contents)))

(def render-xhtml list/widget
  <div (:id ,(id-of -self-) :class "list widget")
    ,(render-context-menu-for -self-)
    ,(call-next-method)>)


;; TODO: move this to list/editor
;;;;;;
;;; List viewer

(def (component e) list/viewer (viewer/basic list/widget)
  ())

;;;;;;
;;; List editor

(def (component e) list/editor (editor/basic list/widget)
  ())

(def layered-method make-context-menu-items ((component list/editor) class prototype value)
  (optional-list* (make-menu-item (make-add-list-element-command component class prototype value) nil)
                  (call-next-method)))

(def (icon e) add-list-element)

(def (layered-function e) make-add-list-element-command (component class prototype value)
  (:method ((component list/editor) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon add-list-element)
      (make-component-action component
        (appendf (contents-of component) (list (add-list-element component class prototype value)))))))

(def (generic e) add-list-element (component class prototype value))

;;;;;;
;;; List inspector

(def (component e) list/inspector (inspector/basic list/editor list/viewer)
  ())

(def (macro e) list/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'list/inspector ,@args :component-value (list ,@forms)))

(def refresh-component list/inspector
  (bind (((:slots contents component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-)))
    (if contents
        (foreach [setf (component-value-of !1) !2] contents component-value)
        (setf contents (mapcar [make-list/element -self- dispatch-class dispatch-prototype !1] component-value)))))

(def (generic e) make-list/element (component class prototype value))
