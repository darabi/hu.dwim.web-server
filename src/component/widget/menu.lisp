;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Menu items mixin

(def (component e) menu-items/mixin ()
  ((menu-items nil :type components))
  (:documentation "A component with a set of menu items."))

;;;;;;
;;; Menu abstract

(def (component e) menu/abstract (menu-items/mixin content/mixin id/mixin style/mixin)
  ()
  (:default-initargs :content (empty))
  (:documentation "A top level component in a menu hierarchy."))

(def (icon e) menu :tooltip nil)

;;;;;;
;;; Menu bar basic

(def (component e) menu-bar/basic (menu/abstract)
  ((target-place nil :type place))
  (:documentation "A menu component that is always shown."))

(def (macro e) menu-bar ((&rest args &key &allow-other-keys) &body menu-items)
  `(make-menu-bar/basic (list ,@menu-items) ,@args))

(def (function e) make-menu-bar/basic (menu-items &key id css-class style)
  (make-instance 'menu-bar/basic
                 :menu-items (flatten menu-items)
                 :id id
                 :css-class css-class
                 :style style))

(def render-xhtml menu-bar/basic
  (bind (((:read-only-slots menu-items id css-class style) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,css-class
            :style ,style
            :dojoType #.+dijit/menu+)
        ,(call-next-method)
        ,(foreach #'render-component menu-items)>)))

;;;;;
;;; Popup menu basic

(def (component e) popup-menu/basic (menu/abstract)
  ()
  (:documentation "A menu component that is shown upon explicit user action."))

(def (function e) make-popup-menu (menu-items &key id css-class style)
  (make-instance 'popup-menu/basic
                 :menu-items (flatten menu-items)
                 :id id
                 :css-class css-class
                 :style style))

(def (macro e) popup-menu (&body menu-items)
  `(make-popup-menu (list ,@menu-items)))

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

(def render-xhtml popup-menu/basic
  (render-popup-menu -self-))

(def render-csv popup-menu/basic
  (write-csv-separated-elements #\Space (commands-of -self-)))

;;;;;
;;; Context menu basic

(def (component e) context-menu/basic (popup-menu/basic)
  ((target))
  (:documentation "A popup menu component that is attached to another component as a context menu."))

(def (macro e) context-menu (target menu-items)
  `(make-instance 'context-menu/basic
                  :target ,target
                  :content (icon show-context-menu)
                  :menu-items (list ,@menu-items)))

(def render-xhtml context-menu/basic
  (render-popup-menu -self- :target-node-id (id-of (target-of -self-))))

(def (icon e) show-context-menu :label nil)

;;;;;;
;;; Context menu mixin

(def (component e) context-menu/mixin ()
  ((context-menu :type component))
  (:documentation "A component with a context menu attached to it."))

(def refresh-component context-menu/mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (context-menu-of -self-) (make-context-menu -self- class prototype value))))

(def layered-method make-context-menu ((component context-menu/mixin) class prototype value)
  (labels ((make-menu-items (elements)
             (iter (for element :in elements)
                   (collect (etypecase element
                              (cons (make-menu-item (icon menu) (make-menu-items element)))
                              (command/basic (make-menu-item element nil))
                              (menu-item/basic
                               (setf (menu-items-of element) (make-menu-items (menu-items-of element)))
                               element))))))
    (make-instance 'context-menu/basic
                   :target component
                   :content (icon show-context-menu)
                   :menu-items (make-menu-items (make-context-menu-items component class prototype value)))))

(def layered-method make-context-menu-items ((component context-menu/mixin) class prototype value)
  nil)

;;;;;;
;;; Menu item basic

(def (component e) menu-item/basic (menu-items/mixin content/mixin id/mixin style/mixin)
  ()
  (:documentation "An intermediate or leaf component in a menu hierarchy."))

(def (macro e) menu-item ((&key id css-class style) content &body menu-items)
  `(make-menu-item ,content (list ,@menu-items) :id ,id :css-class ,css-class :style ,style))

(def (function e) make-menu-item (content menu-items &key id css-class style)
  (bind ((menu-items (flatten menu-items)))
    (when (or menu-items
              content)
      (make-instance 'menu-item/basic
                     :content content
                     :menu-items menu-items
                     :id id
                     :css-class css-class
                     :style style))))

(def render-xhtml menu-item/basic
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
;;; Separator menu item basic

(def (component e) separator-menu-item/basic (menu-item/basic)
  ()
  (:documentation "A menu item separator, a leaf in the menu hierarchy."))

(def render-xhtml separator-menu-item/basic
  <div (:dojoType #.+dijit/menu-separator+)>)

;;;;;;
;;; Replace menu target command component

(def (component e) replace-menu-target-command-component (command/basic)
  ((component))
  (:documentation "A special command that will replace the main menu target place with its component."))

(def constructor replace-menu-target-command-component ()
  (setf (action-of -self-)
        (make-action
          (bind ((menu-component
                  (find-ancestor-component -self-
                                           (lambda (ancestor)
                                             (and (typep ancestor 'menu-bar/basic)
                                                  (target-place-of ancestor)))))
                 (component (force (component-of -self-))))
            (setf (component-of -self-) component
                  (component-at-place (target-place-of menu-component)) component)))))

(def (macro e) replace-menu-target-command (content &body forms)
  `(make-instance 'replace-menu-target-command-component :content ,content :component (delay ,@forms)))
