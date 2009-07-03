;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu abstract

(def (component e) menu/abstract (style/abstract menu-items/mixin)
  ()
  (:documentation "A top level COMPONENT in a MENU hierarchy."))

(def (icon e) menu :tooltip nil)

;;;;;;
;;; Menu bar widget

(def (component e) menu-bar/widget (menu/abstract widget/basic)
  ()
  (:documentation "A MENU that is always shown."))

(def (macro e) menu-bar/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'menu-bar/widget ,@args :menu-items (list ,@menu-items)))

(def (function e) make-menu-bar/widget (menu-items &key id style-class custom-style)
  (make-instance 'menu-bar/widget
                 :menu-items (flatten menu-items)
                 :id id
                 :style-class style-class
                 :custom-style custom-style))

(def render-xhtml menu-bar/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,style-class
            :style ,custom-style
            :dojoType #.+dijit/menu+)
        ,(foreach #'render-component menu-items)>)))

;;;;;
;;; Popup menu widget

(def (component e) popup-menu/widget (menu/abstract widget/basic)
  ()
  (:documentation "A MENU that is shown upon explicit user interaction."))

(def (macro e) popup-menu/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'popup-menu/widget ,@args :menu-items (list ,@menu-items)))

(def (function e) make-popup-menu (menu-items &key id style-class custom-style)
  (make-instance 'popup-menu/widget
                 :menu-items (flatten menu-items)
                 :id id
                 :style-class style-class
                 :style custom-style))

(def function render-popup-menu (component &key target-node-id)
  (bind (((:read-only-slots menu-items id style-class custom-style) component))
    (when menu-items
      <span (:class ,style-class :style ,custom-style)
        ,(render-component (content-of component))
        ,(render-dojo-widget (id)
          <div (:id ,id
                :dojoType #.+dijit/menu+
                :targetNodeIds ,target-node-id
                :style "display: none;")
            ,(foreach #'render-component menu-items)>
          (render-remote-setup component))>)))

(def render-xhtml popup-menu/widget
  (render-popup-menu -self-))

(def render-csv popup-menu/widget
  (write-csv-separated-elements #\Space (menu-items-of -self-)))

;;;;;
;;; Context menu widget

(def (component e) context-menu/widget (popup-menu/widget)
  ((target))
  (:documentation "A POPUP-MENU that is attached to another COMPONENT as a CONTEXT-MENU."))

(def (macro e) context-menu/bacic (target menu-items)
  `(make-instance 'context-menu/widget
                  :target ,target
                  :content (icon show-context-menu)
                  :menu-items (list ,@menu-items)))

(def render-xhtml context-menu/widget
  (render-popup-menu -self- :target-node-id (id-of (target-of -self-))))

(def (icon e) show-context-menu :label nil)

;;;;;;
;;; Menu item widget

(def (component e) menu-item/widget (widget/basic content/abstract style/abstract menu-items/mixin)
  ()
  (:documentation "An intermediate or leaf component in a menu hierarchy."))

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
                     :style custom-style))))

(def render-xhtml menu-item/widget
  (bind (((:read-only-slots menu-items id style-class custom-style) -self-))
    (if menu-items
        (bind ((popup-id (generate-response-unique-string)))
          (render-dojo-widget (popup-id)
            <div (:id ,popup-id
                  :class ,style-class
                  :style ,custom-style
                  :dojoType #.+dijit/popup-menu-item+)
              ,(render-content-for -self-)
              ,(render-dojo-widget (id)
                 <div (:id ,id
                       :dojoType #.+dijit/menu+
                       :style "display: none;")
                   ,(foreach #'render-component menu-items)>
                 (render-remote-setup -self-))>))
        (render-dojo-widget (id)
          <div (:id ,id :dojoType #.+dijit/menu-item+)
            ,(render-content-for -self-)>
          #+nil
          (render-command-onclick-handler command id)))))

;;;;;;
;;; Separator menu item widget

(def (component e) separator-menu-item/widget (menu-item/widget)
  ()
  (:documentation "A menu item separator, a leaf in the menu hierarchy."))

(def render-xhtml separator-menu-item/widget
  <div (:dojoType #.+dijit/menu-separator+)>)
