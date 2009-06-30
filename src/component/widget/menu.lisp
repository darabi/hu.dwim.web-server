;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu abstract

(def (component e) menu/abstract (menu-items/mixin content/mixin id/mixin style/mixin)
  ()
  (:documentation "A top level COMPONENT in a MENU hierarchy."))

(def (icon e) menu :tooltip nil)

;;;;;;
;;; Menu bar widget

(def (component e) menu-bar/widget (menu/abstract)
  ((target-place nil :type place))
  (:documentation "A MENU that is always shown."))

(def (macro e) menu-bar/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-menu-bar/widget (list ,@menu-items) ,@args))

(def (function e) make-menu-bar/widget (menu-items &key id css-class style)
  (make-instance 'menu-bar/widget
                 :menu-items (flatten menu-items)
                 :id id
                 :css-class css-class
                 :style style))

(def render-xhtml menu-bar/widget
  (bind (((:read-only-slots menu-items id css-class style) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,css-class
            :style ,style
            :dojoType #.+dijit/menu+)
        ,(call-next-method)
        ,(foreach #'render-component menu-items)>)))

;;;;;
;;; Popup menu widget

(def (component e) popup-menu/widget (menu/abstract)
  ()
  (:documentation "A MENU that is shown upon explicit user interaction."))

(def (macro e) popup-menu/widget ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-popup-menu (list ,@menu-items) ,@args))

(def (function e) make-popup-menu (menu-items &key id css-class style)
  (make-instance 'popup-menu/widget
                 :menu-items (flatten menu-items)
                 :id id
                 :css-class css-class
                 :style style))

(def function render-popup-menu (component &key target-node-id)
  (bind (((:read-only-slots menu-items id css-class style) component))
    (when menu-items
      <span (:class ,css-class :style ,style)
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

(def (component e) menu-item/widget (menu-items/mixin content/mixin id/mixin style/mixin)
  ()
  (:documentation "An intermediate or leaf component in a menu hierarchy."))

(def (macro e) menu-item ((&key id css-class style) content &body menu-items)
  `(make-menu-item ,content (list ,@menu-items) :id ,id :css-class ,css-class :style ,style))

(def (function e) make-menu-item (content menu-items &key id css-class style)
  (bind ((menu-items (flatten menu-items)))
    (when (or menu-items
              content)
      (make-instance 'menu-item/widget
                     :content content
                     :menu-items menu-items
                     :id id
                     :css-class css-class
                     :style style))))

(def render-xhtml menu-item/widget
  (bind (((:read-only-slots menu-items id css-class style) -self-))
    (if menu-items
        (bind ((popup-id (generate-response-unique-string)))
          (render-dojo-widget (popup-id)
            <div (:id ,popup-id
                  :class ,css-class
                  :style ,style
                  :dojoType #.+dijit/popup-menu-item+)
              ,(call-next-method)
              ,(render-dojo-widget (id)
                 <div (:id ,id
                       :dojoType #.+dijit/menu+
                       :style "display: none;")
                   ,(foreach #'render-component menu-items)>
                 (render-remote-setup -self-))>))
        (render-dojo-widget (id)
          <div (:id ,id :dojoType #.+dijit/menu-item+)
            ,(call-next-method)>
          #+nil
          (render-command-onclick-handler command id)))))

;;;;;;
;;; Separator menu item widget

(def (component e) separator-menu-item/widget (menu-item/widget)
  ()
  (:documentation "A menu item separator, a leaf in the menu hierarchy."))

(def render-xhtml separator-menu-item/widget
  <div (:dojoType #.+dijit/menu-separator+)>)

;;;;;;
;;; Replace menu target command

(def (component e) replace-menu-target-command (command/widget)
  ((component))
  (:documentation "A special command that will replace the main menu target place with its component."))

(def constructor replace-menu-target-command ()
  (setf (action-of -self-)
        (make-action
          (bind ((menu-component
                  (find-ancestor-component -self-
                                           (lambda (ancestor)
                                             (and (typep ancestor 'menu-bar/widget)
                                                  (target-place-of ancestor)))))
                 (component (force (component-of -self-))))
            (setf (component-of -self-) component
                  (component-at-place (target-place-of menu-component)) component)))))

(def (macro e) replace-menu-target-command (content &body forms)
  `(make-instance 'replace-menu-target-command :content ,content :component (delay ,@forms)))
