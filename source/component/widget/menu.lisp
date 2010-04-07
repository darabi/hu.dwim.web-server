;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; menu-bar/widget

(def (icon e) show-submenu)

(def (component e) menu-bar/widget (widget/style menu-items/mixin)
  ()
  (:documentation "A MENU-BAR/WIDGET is always shown with a flat list of MENU-ITEM/WIDGETs immediately VISIBLE."))

(def (macro e) menu-bar/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'menu-bar/widget ,@args :menu-items (flatten (list ,@menu-items))))

(def render-xhtml menu-bar/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,style-class
            :style ,custom-style
            :dojoType #.+dijit/menu-bar+)
        ,(foreach #'render-component menu-items)>)))

;;;;;;
;;; popup-menu/widget

(def (component e) popup-menu/widget (widget/style content/abstract menu-items/mixin)
  ()
  (:documentation "A POPUP-MENU/WIDGET is only shown upon explicit user interaction on its CONTENT."))

(def (macro e) popup-menu/widget ((&rest args &key &allow-other-keys) content &body menu-items)
  `(make-instance 'popup-menu/widget ,@args :content ,content :menu-items (flatten (list ,@menu-items))))

(def render-xhtml popup-menu/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (when menu-items
      <span (:id ,id :class ,style-class :style ,custom-style)
        ,(render-content-for -self-)
        ,(bind ((menu-id (generate-unique-component-id)))
               (render-dojo-widget (menu-id)
          <div (:id ,menu-id
                :dojoType #.+dijit/menu+
                :targetNodeIds ,id
                :style "display: none;")
            ,(foreach #'render-component menu-items)>))>)))

;;;;;;
;;; context-menu/widget

;; dojo context menus are broken on opera up to at least v10.10: http://bugs.dojotoolkit.org/ticket/9227

(def (icon e) show-context-menu)

(def (component e) context-menu/widget (widget/style menu-items/mixin)
  ()
  (:documentation "A CONTEXT-MENU/WIDGET is attached to its PARENT-COMPONENT as its CONTEXT-MENU."))

(def (macro e) context-menu/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'context-menu/widget ,@args :menu-items (flatten (list ,@menu-items))))

(def render-component context-menu/widget
  (values))

(def render-xhtml context-menu/widget
  (bind (((:read-only-slots menu-items id style-class custom-style to-be-rendered-component) -self-)
         (parent-id (id-of (parent-component-of -self-))))
    (when menu-items
      (if (eq to-be-rendered-component :lazy)
          (progn
            <span (:id ,id) "">
            ;; TODO due to the custom :js this hinders the optimization in render-action-js-event-handler
            (flet ((render-context-menu-js (href)
                     `js(bind ((coordinates (create :x event.pageX :y event.pageY)))
                          ;; take a snapshot of the coordinates before a potentially long operation
                          ((wui.io.make-action-event-handler
                            ,href
                            :ajax true
                            :subject-dom-node ,parent-id
                            :send-client-state false
                            :sync true
                            :on-success (lambda ()
                                          ;; TODO don't use dojo internals. find a way to reinvoke the same event, or make sure some other way that the context menu comes up
                                          (._scheduleOpen (dijit.byId ,id) event.target nil coordinates)))
                           event))))
              (render-action-js-event-handler "oncontextmenu" parent-id (make-action
                                                                          (setf (to-be-rendered-component? -self-) #t))
                                              :js #'render-context-menu-js
                                              :one-shot #t :stop-event #t))
            ;; anohter alternative, but left clicking anything will force the context menu to download, and i think :xhr-sync true is also missing...
            #+nil
            (render-action-js-event-handler "onmousedown" parent-id (make-action
                                                                      (setf (to-be-rendered-component? -self-) #t))
                                            :subject-dom-node parent-id
                                            :one-shot #t :sync #t))
          (render-dojo-widget (id)
            <div (:id ,id
                  :class ,style-class
                  :style `str("display: none;" ,custom-style)
                  :dojoType #.+dijit/menu+
                  :targetNodeIds ,parent-id)
              ,(foreach #'render-component menu-items)>)))))

(def method mark-to-be-rendered-component ((self context-menu/widget))
  (setf (to-be-rendered-component? self) :lazy))

(def method mark-rendered-component ((self context-menu/widget))
  (setf (to-be-rendered-component? self) :lazy))

(def method to-be-rendered-component? ((self context-menu/widget))
  (eq (call-next-method) #t))

(def method map-visible-child-components ((component context-menu/widget) function)
  ;; QUESTION to-be-rendered-component? decides about what is visible? sounds weird... comment or fix...
  (when (eq (to-be-rendered-component? component) #t)
    (call-next-method)))

;;;;;;
;;; menu-item/widget

(def (component e) menu-item/widget (widget/style content/abstract menu-items/mixin)
  ()
  (:documentation "A MENU-ITEM/WIDGET is an intermediate or leaf COMPONENT in a MENU-HIERARCHY."))

(def (macro e) menu-item/widget ((&rest args &key &allow-other-keys) content &body menu-items)
  `(make-instance 'menu-item/widget ,@args :content ,content :menu-items (flatten (list ,@menu-items))))

(def (function e) make-menu-item (content &rest menu-items)
  (bind ((menu-items (flatten menu-items)))
    (when (or menu-items
              content)
      (make-instance 'menu-item/widget
                     :content content
                     :menu-items (mapcar (lambda (menu-item)
                                           (if (typep menu-item 'menu-item/widget)
                                               menu-item
                                               (make-menu-item menu-item)))
                                         menu-items)))))

(def (function e) make-submenu-item (content &rest menu-items)
  (bind ((menu-items (flatten menu-items)))
    (when menu-items
      (make-menu-item content menu-items))))

(def render-xhtml menu-item/widget
  (bind (((:read-only-slots menu-items id style-class custom-style content) -self-))
    (if menu-items
        (bind ((popup-id (generate-unique-component-id)))
          (render-dojo-widget (popup-id)
            <div (:id ,popup-id
                  :class ,style-class
                  :style ,custom-style
                  :dojoType ,(if (typep (parent-component-of -self-) 'menu-bar/widget)
                                 #.+dijit/popup-menu-bar-item+
                                 #.+dijit/popup-menu-item+))
              ,(if (stringp (content-of -self-))
                   <span ,(render-content-for -self-)>
                   (render-content-for -self-))
              ,(render-dojo-widget (id)
                 <div (:id ,id
                       :dojoType #.+dijit/menu+
                       :style "display: none;")
                   ,(foreach #'render-component menu-items)>)>))
        (when (visible-component? content)
          (render-dojo-widget (id)
            <div (:id ,id
                  :class ,style-class
                  :style ,custom-style
                  :dojoType #.+dijit/menu-item+
                  :iconClass ,(typecase content
                                (icon/widget
                                 (icon-style-class content))
                                (content/mixin
                                 (bind ((content-content (content-of content)))
                                   (when (typep content-content 'icon/widget)
                                     (icon-style-class content-content))))
                                (t
                                 nil)))
              ,(typecase content
                 (icon/widget
                  (render-component (force (label-of content))))
                 (content/mixin
                  (bind ((content-content (content-of content)))
                    (if (typep content-content 'icon/widget)
                        (render-component (force (label-of (content-of content))))
                        (render-component content))))
                 (t
                  (render-component content)))>
            (when (typep content 'command/widget)
              (render-command-onclick-handler content id)))))))

(def function render-show-context-menu-command-for (component)
  (declare (ignore component))
  ;; TODO: add js to really show the menu
  (render-component (icon/widget show-context-menu :label nil)))

(def method command-position ((self menu-item/widget))
  (if (menu-items-of self)
      most-positive-fixnum
      (command-position (content-of self))))

;;;;;;
;;; menu-item-separator/widget

(def (component e) menu-item-separator/widget (widget/style)
  ()
  (:documentation "A MENU-ITEM-SEPARATOR/WIDGET is a leaf COMPONENT in the MENU-HIERARCHY separating other MENU-ITEM/WIDGETs."))

(def (macro e) menu-item-separator/widget (&rest args &key &allow-other-keys)
  `(make-instance 'menu-item-separator/widget ,@args))

(def render-xhtml menu-item-separator/widget
  (bind (((:read-only-slots id) -self-))
    (render-dojo-widget (id)
      <div (:id ,id :dojoType #.+dijit/menu-separator+)
        ;; NOTE: we do need the empty string in the body to workaround a bug in firefox
        ;;       that occurs when the element is rendered without an explicit closing tag
        "">)))
