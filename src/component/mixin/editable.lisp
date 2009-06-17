;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;

;;;;;;
;;; Editable mixin

(def (component ea) editable/mixin ()
  ((edited #f :type boolean :documentation "TRUE indicates the component is currently being edited, FALSE otherwise."))
  (:documentation "
A component that supports the editing protocols.

The generic commands BEGIN-EDITING, SAVE-EDITING, CANCEL-EDITING,
STORE-EDITING, REVERT-EDITING do their job recursively within the
scope of the component for which they are created.

Components which are edited should not be made invisible not to
confuse the user. For example collapsing a detail to a reference
is not allowed when the detail is being edited. The user must
first do a SAVE-EDITING or CANCEL-EDITING before being able to
collapse or hide by any other means.

Components may be created immediately being edited, moreover
STORE-EDITING and REVERT-EDITING can be used instead of SAVE-EDITING
and CANCEL-EDITING to continuously leave the component in edit mode.
"))

(def refresh editable/mixin
  (if (edited? -self-)
      (join-editing -self-)
      (leave-editing -self-)))

(def layered-method begin-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (join-editing self))

(def layered-method save-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (store-editing editable)
  (when leave-editing
    (leave-editing editable)))

(def layered-method cancel-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (revert-editing editable)
  (leave-editing editable))

(def layered-method make-context-menu-items ((component editable/mixin) (class standard-class) (prototype standard-object) (instance standard-object))
  (append (list (make-menu-item (icon menu :label "Szerkesztés")
                                (make-editing-commands component class prototype instance)))
          (call-next-method)))

;;;;;;
;;; Traverse

(def generic map-editable-child-components (component function)
  (:method ((component component) function)
    (ensure-functionf function)
    (map-child-components component (lambda (child)
                                      (when (typep child 'editable/mixin)
                                        (funcall function child))))))

(def function map-editable-descendant-components (component function)
  (ensure-functionf function)
  (map-editable-child-components component (lambda (child)
                                             (funcall function child)
                                             (map-editable-descendant-components child function))))

(def function find-editable-child-component (component function)
  (ensure-functionf function)
  (map-editable-child-components component (lambda (child)
                                             (when (funcall function child)
                                               (return-from find-editable-child-component child))))
  nil)

(def function find-editable-descendant-component (component function)
  (map-editable-descendant-components component (lambda (descendant)
                                                  (when (funcall function descendant)
                                                    (return-from find-editable-descendant-component descendant))))
  nil)

(def function has-edited-child-component-p (component)
  (find-editable-child-component component 'edited?))

(def function has-edited-descendant-component-p (component)
  (find-editable-descendant-component component 'edited?))

;;;;;;
;;; Customization points

(def layered-methods join-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'join-editing))

  (:method :before ((component editable/mixin))
    (setf (edited? component) #t))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def layered-methods leave-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'leave-editing))

  (:method :before ((component editable/mixin))
    (setf (edited? component) #f))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def layered-methods store-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'store-editing))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def layered-methods revert-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'revert-editing))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

;;;;;;
;;; Command

(def layered-method make-begin-editing-command ((component editable/mixin) class prototype value)
  (command (:visible (or visible (delay (not (edited? component)))))
    (icon begin-editing)
    (make-component-action component
      (with-interaction component
        (begin-editing component)))))

(def layered-method make-save-editing-command (component class prototype value)
  (command (:visible (delay (edited? component)))
    (icon save-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-cancel-editing-command ((component editable/mixin) class prototype value)
  (command (:visible (delay (edited? component)))
    (icon cancel-editing)
    (make-component-action component
      (with-interaction component
        (cancel-editing component)))))

(def layered-method make-store-editing-command ((component editable/mixin) class prototype value)
  (command (:visible (delay (edited? component)))
    (icon store-editing)
    (make-component-action component
      (with-interaction component
        (save-editing component)))))

(def layered-method make-revert-editing-command ((component editable/mixin) class prototype instance)
  (command (:visible (delay (edited? component)))
    (icon revert-editing)
    (make-component-action component
      (with-interaction component
        (revert-editing component)))))

(def layered-method make-editing-commands ((component editable/mixin) class prototype instance)
  (if (inherited-initarg component :store-mode)
      (list (make-store-editing-command component)
            (make-revert-editing-command component))
      (list (make-begin-editing-command component)
            (make-save-editing-command component)
            (make-cancel-editing-command component))))

(def layered-method make-refresh-command ((component editable/mixin) class prototype instance)
  (command (:visible (delay (not (edited? component)))
            :ajax (ajax-id component))
    (icon refresh)
    (make-component-action component
      (refresh-component component))))

;;;;;;
;;; Icon

(def icon begin-editing)

(def icon save-editing)

(def icon cancel-editing)

(def icon store-editing)

(def icon revert-editing)

;;;;;;
;;; Localization

(def resources hu
  (icon-label.begin-editing "Szerkesztés")
  (icon-tooltip.begin-editing "Szerkesztés elkezdése")

  (icon-label.save-editing "Mentés")
  (icon-tooltip.save-editing "Változtatások mentése és a szerkesztés befejezése")

  (icon-label.cancel-editing "Elvetés")
  (icon-tooltip.cancel-editing "Változtatások elvetése és a szerkesztés befejezése")

  (icon-label.store-editing "Mentés")
  (icon-tooltip.store-editing "Változtatások mentése")

  (icon-label.revert-editing "Elvetés")
  (icon-tooltip.revert-editing "Változtatások elvetése"))

(def resources en
  (icon-label.begin-editing "Edit")
  (icon-tooltip.begin-editing "Start editing")

  (icon-label.save-editing "Save")
  (icon-tooltip.save-editing "Save changes and finish editing")

  (icon-label.cancel-editing "Cancel")
  (icon-tooltip.cancel-editing "Cancel changes and finish editing")

  (icon-label.store-editing "Store")
  (icon-tooltip.store-editing "Store changes")

  (icon-label.revert-editing "Revert")
  (icon-tooltip.revert-editing "Revert changes"))
