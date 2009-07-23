;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; menu-bar/widget

(def (icon e) show-submenu)

(def (component e) menu-bar/widget (widget/style menu-items/mixin)
  ()
  (:documentation "A MENU-BAR/WIDGET is always shown with a list of MENU-ITEM/WIDGETs VISIBLE."))

(def (macro e) menu-bar/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'menu-bar/widget ,@args :menu-items (list ,@menu-items)))

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
  `(make-instance 'popup-menu/widget ,@args :content ,content :menu-items (list ,@menu-items)))

(def render-xhtml popup-menu/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (when menu-items
      <span (:id ,id :class ,style-class :style ,custom-style)
        ,(render-content-for -self-)
        ,(bind ((menu-id (generate-response-unique-string)))
               (render-dojo-widget (menu-id)
          <div (:id ,menu-id
                :dojoType #.+dijit/menu+
                :targetNodeIds ,id
                :style "display: none;")
            ,(foreach #'render-component menu-items)>))>)))

;;;;;;
;;; context-menu/widget

(def (icon e) show-context-menu)

(def (component e) context-menu/widget (widget/style menu-items/mixin)
  ()
  (:documentation "A CONTEXT-MENU/WIDGET is attached to its PARENT-COMPONENT as its CONTEXT-MENU."))

(def (macro e) context-menu/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'context-menu/widget ,@args :menu-items (list ,@menu-items)))

(def render-xhtml context-menu/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (when menu-items
      (render-dojo-widget (id)
        <div (:id ,id
              :class ,style-class
              :style `str("display: none;" ,custom-style)
              :dojoType #.+dijit/menu+
              :targetNodeIds ,(id-of (parent-component-of -self-)))
          ,(foreach #'render-component menu-items)>))))

;;;;;;
;;; menu-item/widget

(def (component e) menu-item/widget (widget/style content/abstract menu-items/mixin)
  ()
  (:documentation "A MENU-ITEM/WIDGET is an intermediate or leaf COMPONENT in a MENU-HIERARCHY."))

(def (macro e) menu-item/widget ((&rest args &key &allow-other-keys) content &body menu-items)
  `(make-instance 'menu-item/widget ,@args :content ,content :menu-items (list ,@menu-items)))

(def (function e) make-menu-item (content menu-items &key id style-class custom-style)
  (bind ((menu-items (flatten menu-items)))
    (when (or menu-items
              content)
      (make-instance 'menu-item/widget
                     :content content
                     :menu-items menu-items
                     :id id
                     :style-class style-class
                     :custom-style custom-style))))

(def render-xhtml menu-item/widget
  (bind (((:read-only-slots menu-items id style-class custom-style content) -self-))
    (if menu-items
        (bind ((popup-id (generate-response-unique-string)))
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
        (render-dojo-widget (id)
          <div (:id ,id
                :class ,style-class
                :style ,custom-style
                :dojoType #.+dijit/menu-item+)
            ,(render-content-for -self-) >
          (when (typep content 'command/widget)
            (render-command-onclick-handler content id))))))

(def function render-show-context-menu-command-for (component)
  ;; TODO: add js to really show the menu
  (render-component (icon show-context-menu :label nil)))

;;;;;;
;;; menu-item-separator/widget

(def (component e) menu-item-separator/widget (widget/style)
  ()
  (:documentation "A MENU-ITEM-SEPARATOR/WIDGET is a leaf COMPONENT in the MENU-HIERARCHY separating other MENU-ITEM/WIDGETs."))

(def (macro e) menu-item-separator/widget ()
  '(make-instance 'menu-item-separator/widget))

(def render-xhtml menu-item-separator/widget
  (bind (((:read-only-slots id) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :dojoType #.+dijit/menu-separator+)
        ;; NOTE: firefox messes up divs without content
        "">)))
