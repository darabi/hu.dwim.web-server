;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Menu items mixin

(def component menu-items-mixin ()
  ((menu-items nil :type components :export :accessor))
  (:documentation "A component with a set of menu items."))

;;;;;;
;;; Menu mixin

(def (component ea) menu-mixin (menu-items-mixin content-mixin remote-identity-mixin style-mixin)
  ()
  (:default-initargs :content (empty))
  (:documentation "A top level component in a menu hierarchy."))

(def icon menu :tooltip nil)

;;;;;;
;;; Main menu component

(def (component ea) main-menu-component (menu-mixin)
  ((target-place nil :type place))
  (:documentation "A menu component that is always shown."))

(def (function e) make-main-menu (menu-items &key id css-class style)
  (make-instance 'main-menu-component
                 :menu-items (flatten menu-items)
                 :id id
                 :css-class css-class
                 :style style))

(def (macro e) menu (&body menu-items)
  `(make-main-menu (list ,@menu-items)))

(def render-xhtml main-menu-component
  (bind (((:read-only-slots menu-items id css-class style) -self-))
    (render-dojo-widget (id)
      <div (:id ,id
            :class ,css-class
            :style ,style
            :dojoType #.+dijit/menu+)
        ,(call-next-method)
        ,(foreach #'render menu-items)>)))

;;;;;
;;; Popup menu component

(def component popup-menu-component (menu-mixin)
  ()
  (:documentation "A menu component that is shown upon explicit user action."))

(def (function e) make-popup-menu (menu-items &key id css-class style)
  (make-instance 'popup-menu-component
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
        ,(render (content-of component))
        ,(render-dojo-widget (id)
          <div (:id ,id
                :dojoType #.+dijit/menu+
                :targetNodeIds ,target-node-id
                :style "display: none;")
            ,(foreach #'render menu-items)>
          (render-remote-setup component))>)))

(def render-xhtml popup-menu-component
  (render-popup-menu -self-))

(def render-csv popup-menu-component
  (render-csv-separated-elements #\Space (commands-of -self-)))

;;;;;
;;; Context menu component

(def component context-menu-component (popup-menu-component)
  ((target))
  (:documentation "A popup menu component that is attached to another component as a context menu."))

(def (macro e) context-menu (target menu-items)
  `(make-instance 'context-menu-component
                  :target ,target
                  :content (icon show-context-menu)
                  :menu-items (list ,@menu-items)))

(def render-xhtml context-menu-component
  (render-popup-menu -self- :target-node-id (id-of (target-of -self-))))

(def icon show-context-menu :label nil)
(def resources hu
  (icon-tooltip.show-context-menu "Környezetfüggő menü megjelenítése"))
(def resources en
  (icon-tooltip.show-context-menu "Show context menu"))

;;;;;;
;;; Context menu mixin

(def component context-menu-mixin ()
  ((context-menu :type component))
  (:documentation "A component with a context menu attached to it."))

(def refresh context-menu-mixin
  (bind ((class (component-dispatch-class -self-))
         (prototype (when class (class-prototype class))))
    (setf (context-menu-of -self-) (make-context-menu -self- class prototype (component-value-of -self-)))))

(def (layered-function e) make-context-menu (component class prototype value)
  (:method ((component context-menu-mixin) class prototype value)
    (labels ((make-menu-items (elements)
               (iter (for element :in elements)
                     (collect (etypecase element
                                (cons (make-menu-item (icon menu) (make-menu-items element)))
                                (command-component (make-menu-item element nil))
                                (menu-item-component
                                 (setf (menu-items-of element) (make-menu-items (menu-items-of element)))
                                 element))))))
      (make-instance 'context-menu-component
                     :target component
                     :content (icon show-context-menu)
                     :menu-items (make-menu-items (make-context-menu-items component class prototype value))))))

(def (layered-function e) make-context-menu-items (component class prototype value)
  (:method ((component component) class prototype value)
    nil)

  (:method ((component component) (class standard-class) (prototype standard-object) value)
    (append (call-next-method)
            (list (make-menu-item (icon menu :label "Mozgatás")
                                  (make-move-commands component class prototype value)))))

  (:method ((component inspector-component) (class standard-class) (prototype standard-object) value)
    (optional-list* (make-refresh-command component class prototype value) (call-next-method)))

  (:method ((component inspector-component) (class built-in-class) (prototype null) value)
    nil))

;;;;;;
;;; Menu item component

(def component menu-item-component (menu-items-mixin content-mixin remote-identity-mixin style-mixin)
  ()
  (:documentation "An intermediate component in a menu hierarchy."))

(def (function e) make-menu-item (content menu-items &key id css-class style)
  (bind ((menu-items (flatten menu-items)))
    (when (or menu-items
              content)
      (make-instance 'menu-item-component
                     :content content
                     :menu-items menu-items
                     :id id
                     :css-class css-class
                     :style style))))

(def (macro e) menu-item ((&key id css-class style) content &body menu-items)
  `(make-menu-item ,content (list ,@menu-items) :id ,id :css-class ,css-class :style ,style))

(def render-xhtml menu-item-component
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
                   ,(foreach #'render menu-items)>
                 (render-remote-setup -self-))>))
        (render-dojo-widget (id)
          <div (:id ,id :dojoType #.+dijit/menu-item+)
            ,(call-next-method)>
          #+nil
          (render-command-onclick-handler command id)))))

;;;;;;
;;; Separator menu item component

(def component separator-menu-item-component (menu-item-component)
  ()
  (:documentation "A menu item separator, a leaf in the menu hierarchy."))

(def render-xhtml separator-menu-item-component
  <div (:dojoType #.+dijit/menu-separator+)>)

;;;;;;
;;; Replace menu target command component

(def component replace-menu-target-command-component (command-component)
  ((component))
  (:documentation "A special command that will replace the main menu target place with its component."))

(def constructor replace-menu-target-command-component ()
  (setf (action-of -self-)
        (make-action
          (bind ((menu-component
                  (find-ancestor-component -self-
                                           (lambda (ancestor)
                                             (and (typep ancestor 'main-menu-component)
                                                  (target-place-of ancestor)))))
                 (component (force (component-of -self-))))
            (setf (component-of -self-) component
                  (component-at-place (target-place-of menu-component)) component)))))

(def (macro e) replace-menu-target-command (content &body forms)
  `(make-instance 'replace-menu-target-command-component :content ,content :component (delay ,@forms)))

;;;;;;
;;; Debug menu

(def (function e) make-debug-menu ()
  (menu-item () "Debug"
    (menu-item ()
        (command "Start over"
                 (make-action (reset-frame-root-component))
                 :send-client-state #f))
    (menu-item ()
        (command "Toggle test mode"
                 (make-action (notf (running-in-test-mode? *application*)))))
    (menu-item ()
        (command "Toggle profiling"
                 (make-action (notf (profile-request-processing? *server*)))))
    (menu-item ()
        (command "Toggle hierarchy"
                 (make-action (toggle-debug-component-hierarchy *frame*))))
    (menu-item ()
        (command "Toggle debug client side"
                 (make-action (notf (debug-client-side? (root-component-of *frame*))))))
    ;; from http://turtle.dojotoolkit.org/~david/recss.html
    (menu-item ()
        (inline-component
          <a (:href "#"
              :class "command"
              :onClick `js-inline(wui.reload-css))
             "Reload CSS">))
    #+sbcl
    (menu-item ()
        (replace-menu-target-command "Frame size breakdown"
          (make-instance 'frame-size-breakdown-component)))
    (menu-item ()
        (replace-menu-target-command "Web server"
          (make-instance 'server-info)))))
