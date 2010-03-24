;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.

;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)


;; TODO: move
(def method component-style-class ((self title/widget))
  (string+ "title-border " (call-next-method)))

(def method component-style-class ((self title-bar/widget))
  (string+ "title-border " (call-next-method)))

(def method component-style-class ((self usage-help/widget))
  (string+ "content-border " (call-next-method)))

(def method component-style-class ((self table/widget))
  (string+ "content-border " (call-next-method)))

(def method component-style-class ((self tree/widget))
  (string+ "content-border " (call-next-method)))

(def method component-style-class ((self treeble/widget))
  (string+ "content-border " (call-next-method)))

(def method component-style-class ((self t/presentation))
  (string+ "content-border " (call-next-method)))

(def method component-style-class ((self t/detail/presentation))
  (%component-style-class self))

(def method component-style-class ((self column/widget))
  (string+ "table-header-border " (call-next-method)))

(def method component-style-class ((self text/inspector))
  "text inspector")

(def method component-style-class ((self book/inspector))
  "content-border text inspector")

(def method component-style-class ((self uri/inspector))
  "uri inspector")

(def method component-style-class ((self hyperlink/inspector))
  "hyperlink inspector")










(def function make-copy-to-repl-command (component)
  (command/widget (:ajax #t)
    "COPY"
    (make-action (copy-to-repl component))))

(def function make-inspect-in-repl-command (component)
  (command/widget (:ajax #t)
    "INSPECT"
    (make-action (inspect-in-repl component))))

(def (icon e) navigate-back)

(def (icon e) external-link)

;;;;;;
;;; Icon

(def (icon e) diagram)

(def (icon e) hide-component)

(def (icon e) show-component)

(def (icon e) refresh-component)

(def (icon e) select-component)

(def (icon e) switch-to-alternative)

(def layered-method make-refresh-component-command ((component refreshable/mixin) class prototype value)
  (command/widget ()
    (icon/widget refresh-component)
    (make-component-action component
      (refresh-component component))))

(def layered-method make-select-component-command ((component selectable/mixin) class prototype value)
  (command/widget (:ajax (awhen (find-selection-component component) (ajax-of it))
                   :enabled (delay (selectable-component? component)))
    (icon/widget select-component)
    (make-component-action component
      (notf (selected-component? (find-selection-component component) component)))))

(def layered-method make-context-menu-items ((component selectable/mixin) class prototype value)
  (optional-list* (make-menu-item (make-select-component-command component class prototype value))
                  (call-next-method)))

(def function command-with-icon-name? (component name)
  (and (typep component 'command/widget)
       (bind ((content (content-of component)))
         (and (typep content 'icon/widget)
              (eq name (name-of content))))))

(def (generic e) find-command (component name)
  (:method ((self component) name)
    nil)

  (:method ((self context-menu/mixin) name)
    (or (call-next-method)
        (find-descendant-component (context-menu-of self)
                                   (lambda (descendant)
                                     (command-with-icon-name? descendant name)))))

  (:method ((self menu-bar/mixin) name)
    (or (call-next-method)
        (find-descendant-component (menu-bar-of self)
                                   (lambda (descendant)
                                     (command-with-icon-name? descendant name)))))

  (:method ((self command-bar/mixin) name)
    (or (call-next-method)
        (find-child-component (command-bar-of self)
                              (lambda (child)
                                (command-with-icon-name? child name))))))

(def (function e) render-hide-command-for (component)
  (render-component (command/widget ()
                      (icon/widget hide-component :label nil)
                      (make-action
                        (hide-component component)))))

;;;;;;
;;; Icon

(def (icon e) begin-editing)

(def (icon e) save-editing)

(def (icon e) cancel-editing)

(def (icon e) store-editing)

(def (icon e) revert-editing)

(def layered-method make-context-menu-items ((component editable/mixin) (class standard-class) (prototype standard-object) (instance standard-object))
  (optional-list* (make-submenu-item (icon/widget menu :label "Edit") (make-editing-commands component class prototype instance))
                  (call-next-method)))

(def layered-method make-command-bar-commands ((component editable/mixin) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (when (editable-component? component)
            (list (make-save-editing-command component class prototype instance)
                  (make-cancel-editing-command component class prototype instance)))
          (call-next-method)))

;;;;;;
;;; Editable

(def layered-method make-begin-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (or (editable-component? component)
                                (delay (not (edited-component? component))))
                   :ajax (ajax-of component))
    (icon/widget begin-editing)
    (make-component-action component
      (with-interaction component
        (begin-editing component)))))

(def layered-method make-save-editing-command (component class prototype value)
  (command/widget (:visible (delay (edited-component? component))
                   :ajax (ajax-of component))
    (icon/widget save-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-cancel-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (delay (edited-component? component))
                   :ajax (ajax-of component))
    (icon/widget cancel-editing)
    (make-component-action component
      (with-interaction component
        (cancel-editing component)))))

(def layered-method make-store-editing-command ((component editable/mixin) class prototype value)
  (command/widget (:visible (delay (edited-component? component))
                   :ajax (ajax-of component))
    (icon/widget store-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-revert-editing-command ((component editable/mixin) class prototype instance)
  (command/widget (:visible (delay (edited-component? component))
                   :ajax (ajax-of component))
    (icon/widget revert-editing)
    (make-component-action component
      (with-interaction component
        (revert-editing component)))))

(def layered-method make-editing-commands ((component editable/mixin) class prototype instance)
  (cond ((editable-component? component)
         (list (make-begin-editing-command component class prototype instance)
               (make-save-editing-command component class prototype instance)
               (make-cancel-editing-command component class prototype instance)))
        ((edited-component? component)
         (list (make-store-editing-command component class prototype instance)
               (make-revert-editing-command component class prototype instance)))))

(def layered-method make-refresh-component-command ((component editable/mixin) class prototype instance)
  (command/widget (:visible (delay (not (edited-component? component)))
                   :ajax (ajax-of component))
    (icon/widget refresh-component)
    (make-component-action component
      (refresh-component component))))

(def function extract-primitive-component-place (component)
  (bind ((parent-component (parent-component-of component)))
    (when (typep parent-component 'inspector/abstract)
      (bind ((component-value (component-value-of parent-component)))
        (when (typep component-value 'object-slot-place)
          (bind ((instance (instance-of component-value)))
            (values (class-of instance) instance (slot-of component-value))))))))

;;;;;
;;; Exportable

(def layered-method make-context-menu-items ((component exportable/abstract) class prototype instance)
  (optional-list* (make-submenu-item (icon/widget menu :label "Export")
                                     (make-export-commands component class prototype instance))
                  (call-next-layered-method)))

(def (icon e) export-text)

(def layered-method make-export-command ((format (eql :txt)) (component exportable/abstract) class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-text)
    (make-component-action component
      (export-text component))))

(def (icon e) export-csv)

(def layered-method make-export-command ((format (eql :csv)) (component exportable/abstract) class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-csv)
    (make-component-action component
      (export-csv component))))

(def (icon e) export-pdf)

(def special-variable *pdf-stream*)

(def layered-method make-export-command ((format (eql :pdf)) (component exportable/abstract) class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-pdf)
    (make-component-action component
      (export-pdf component))))

(def (icon e) export-odt)

(def layered-method make-export-command ((format (eql :odt)) (component exportable/abstract) class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-odt)
    (make-component-action component
      (export-odt component))))

(def (icon e) export-ods)

(def layered-method make-export-command ((format (eql :ods)) (component exportable/abstract) class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-ods)
    (make-component-action component
      (export-ods component))))

(def (icon e) export-sh)

(def layered-method make-export-command ((format (eql :sh)) component class prototype value)
  (command/widget (:ajax #f :delayed-content #t :application-relative-path (export-file-name format component value))
    (icon/widget export-sh)
    (make-component-action component
      (export-sh component))))

;;;;;;
;;; Cloneable

(def (icon e) open-in-new-frame)

(def layered-method make-open-in-new-frame-command ((component component) class prototype value)
  (command/widget (:delayed-content #t :js (lambda (href) `js(window.open ,href)))
    (icon/widget open-in-new-frame)
    (make-component-action component
      (open-in-new-frame component class prototype value))))

(def (icon e) focus-in)

(def (icon e) focus-out)

(def layered-method make-focus-command ((component component) class prototype value)
  (bind ((original-component (delay (find-top-component-content component))))
    (make-replace-and-push-back-command original-component component
                                        (list :content (icon/widget focus-in) :visible (delay (not (top-component-content? component))))
                                        (list :content (icon/widget focus-out)))))

(def layered-method make-context-menu-items ((component command-bar/mixin) class prototype value)
  (optional-list* (make-submenu-item (icon/widget menu :label "Move") (make-move-commands component class prototype value))
                  (call-next-method)))

(def layered-method make-move-commands ((component command-bar/mixin) class prototype value)
  (optional-list* (make-focus-command component class prototype value) (call-next-method)))

;;;;;
;;; Closeable

(def (icon e) close-component)

(def layered-method make-close-component-command ((component closable/abstract) class prototype value)
  (command/widget ()
    (icon/widget close-component)
    (make-component-action component
      (close-component component class prototype value))))

#|

;;;;;;
;;; Command

(def layered-method make-context-menu-items ((component component) class prototype value)
  (append (call-next-method)
          (list (make-menu-item (icon/widget menu :label #"context-menu.move-commands")
                                (make-move-commands component class prototype value)))))

;;;;;;
;;; Component value basic

(def (component e) component-value/basic (cloneable/mixin)
  ())

(def method clone-component :around ((self component-value/basic))
  ;; this must be done at the very last, after all primary method customization
  (prog1-bind clone (call-next-method)
    (setf (component-value-of clone) (component-value-of self))))



;;;;;;
;;; Command

(def layered-method make-hide-command ((component hideable/mixin) class prototype value)
  (command/widget ()
    (icon/widget hide-component)
    (make-component-action component
      (hide-component component))))

(def layered-method make-show-command ((component hideable/mixin) class prototype value)
  (command/widget ()
    (icon/widget show-component)
    (make-component-action component
      (show-component component))))

(def layered-method make-show-component-recursively-command ((component hideable/mixin) class prototype value)
  (command/widget ()
    (icon/widget show-component)
    (make-component-action component
      (show-component-recursively component))))

(def layered-method make-toggle-visiblity-command ((component hideable/mixin) class prototype value)
  (command/widget ()
    (if (visible-component? component)
        (icon/widget hide-component)
        (icon/widget show-component))
    (make-component-action component
      (if (visible-component? component)
          (hide-component component)
          (show-component component)))))

(def layered-method make-context-menu-items ((component hideable/mixin) class prototype value)
  (list* (menu-item ()
             (icon/widget menu :label "Show/Hide")
           (make-hide-command component class prototype value)
           (make-show-component-recursively-command component class prototype value))
         (call-next-method)))

;;;;;;
;;; Default icons
;;; TODO: move the icons where they are actually used

(def (icon e) new)

(def (icon e) create)

(def (icon e) delete)

(def (icon e) close)

(def (icon e) expand)

(def (icon e) collapse)

(def (icon e) filter)

(def (icon e) find)

(def (icon e) set-to-nil)

(def (icon e) set-to-unbound)

(def (icon e) select)

(def (icon e) finish)

(def (icon e) cancel)
|#
