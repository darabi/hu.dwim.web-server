;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu abstract

(def (component e) menu/abstract (style/abstract menu-items/mixin)
  ()
  (:documentation "A MENU/ABSTRACT is a top level COMPONENT in a MENU-HIERARCHY."))

(def (icon e) menu :tooltip nil)

;;;;;;
;;; Menu bar widget

(def (component e) menu-bar/widget (menu/abstract widget/basic)
  ()
  (:documentation "A MENU-BAR/WIDGET is a specail MENU/ABSTRACT that is always shown with a list of MENU-ITEM/WIDGETs."))

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
            :dojoType #.+dijit/menu-bar+)
        ,(foreach #'render-component menu-items)>)))

;;;;;;
;;; Popup menu widget

(def (component e) popup-menu/widget (menu/abstract widget/basic content/abstract)
  ()
  (:documentation "A POPUP-MENU/WIDGET is a special MENU/ABSTRACT that is only shown upon explicit user interaction."))

(def (macro e) popup-menu/widget ((&rest args &key &allow-other-keys) content &body menu-items)
  `(make-instance 'popup-menu/widget ,@args
                  :content ,content
                  :menu-items (list ,@menu-items)))

(def (function e) make-popup-menu (menu-items &key id style-class custom-style)
  (make-instance 'popup-menu/widget
                 :menu-items (flatten menu-items)
                 :id id
                 :style-class style-class
                 :custom-style custom-style))

(def function render-popup-menu (component &key target-node-id)
  (bind (((:read-only-slots menu-items id style-class custom-style) component))
    (when menu-items
      <span (:class ,style-class :style ,custom-style)
        ,(render-content-for component)
        ,(render-dojo-widget (id)
          <div (:id ,id
                :dojoType #.+dijit/menu+
                :targetNodeIds ,target-node-id
                :style "display: none;")
            ,(foreach #'render-component menu-items)>)>)))

(def render-xhtml popup-menu/widget
  (render-popup-menu -self-))

(def render-csv popup-menu/widget
  (write-csv-separated-elements #\Space (menu-items-of -self-)))

;;;;;;
;;; Context menu widget

(def (component e) context-menu/widget (popup-menu/widget)
  ((content (icon show-context-menu))
   (target nil :type t))
  (:documentation "A CONTEXT-MENU/WIDGET is a special POPUP-MENU/WIDGET that is attached to another COMPONENT as its CONTEXT-MENU."))

(def (macro e) context-menu/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-instance 'context-menu/widget ,@args
                  :menu-items (list ,@menu-items)))

(def refresh-component context-menu/widget
  (bind (((:slots target) -self-))
    (unless target
      (setf target (parent-component-of -self-)))))

(def render-xhtml context-menu/widget
  (render-popup-menu -self- :target-node-id (id-of (target-of -self-))))

(def (icon e) show-context-menu :label nil)

;;;;;;
;;; Menu item widget

(def (component e) menu-item/widget (widget/basic content/abstract style/abstract menu-items/mixin)
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
          <div (:id ,id :dojoType #.+dijit/menu-item+)
            ,(render-content-for -self-) >
          (when (typep content 'command/widget)
            (render-command-onclick-handler content id))))))

;;;;;;
;;; Separator menu item widget

(def (component e) separator-menu-item/widget (menu-item/widget)
  ()
  (:documentation "A SEPARATOR-MENU-ITEM/WIDGET is a special MENU-ITEM/WIDGET, a leaf COMPONENT in the MENU-HIERARCHY."))

(def render-xhtml separator-menu-item/widget
  <div (:dojoType #.+dijit/menu-separator+)>)
