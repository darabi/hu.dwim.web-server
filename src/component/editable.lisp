;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;;;
;;;; The generic commands Edit, Save, Cancel, Store, Revert do their
;;;; job recursively within the scope of the component for which they
;;;; are created.
;;;;
;;;; Components which are edited should not be made invisible not to
;;;; confuse the user. For example collapsing a detail to a reference
;;;; is not allowed when the detail is being edited.  The user must
;;;; first do a Save or Cancel before being able to collapse or hide
;;;; by any other means.
;;;;
;;;; Components may be created immediately being edited, moreover
;;;; Store and Revert can be used instead of Save and Cancel to leave
;;;; the component in edit mode.

;;;;;;
;;; Editable

(def component editable-component ()
  ((edited #f :type boolean :documentation "TRUE indicates the component is currently being edited, FALSE otherwise.")))

(def (function e) begin-editing (editable)
  (assert (typep editable 'editable-component))
  (join-editing editable))

(def (function e) save-editing (editable)
  (assert (typep editable 'editable-component))
  (store-editing editable)
  (leave-editing editable))

(def (function e) cancel-editing (editable)
  (assert (typep editable 'editable-component))
  (revert-editing editable)
  (leave-editing editable))

(def generic map-editable-child-components (component function)
  (:method ((component component) function)
    (map-child-components component (lambda (child)
                                      (when (typep child 'editable-component)
                                        (funcall function child))))))

(def function map-editable-descendant-components (component function)
  (map-editable-child-components component (lambda (child)
                                             (funcall function child)
                                             (map-editable-descendant-components child function))))

(def function find-editable-child-component (component function)
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
  (find-editable-child-component component #'edited-p))

(def function has-edited-descendant-component-p (component)
  (find-editable-descendant-component component #'edited-p))

;;;;;;
;;; Customization points

(def generic join-editing (component)
  (:method ((component component))
    (setf (edited-p component) #t)
    (map-editable-child-components component #'join-editing)))

(def generic leave-editing (component)
  (:method ((component component))
    (setf (edited-p component) #f)
    (map-editable-child-components component #'leave-editing)))

(def generic store-editing (component)
  (:method ((component component))
    (map-editable-child-components component #'store-editing)))

(def generic revert-editing (component)
  (:method ((component component))
    (map-editable-child-components component #'revert-editing)))

;;;;;;
;;; Commands

(def (function e) make-begin-editing-command (editable)
  "The BEGIN-EDITING command starts editing underneath the given EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (make-instance 'command-component
                 :icon (clone-icon 'edit)
                 :visible (delay (not (edited-p editable)))
                 :action (make-action (begin-editing editable))))

(def (function e) make-save-editing-command (editable)
  "The SAVE-EDITING command actually makes the changes present under an EDITABLE-COMPNENT and leaves editing"
  (assert (typep editable 'editable-component))
  (make-instance 'command-component
                 :icon (clone-icon 'save)
                 :visible (delay (edited-p editable))
                 :action (make-action (save-editing editable)
                                      (add-user-message editable "Successfully saved changes"))))

(def (function e) make-cancel-editing-command (editable)
  "The CANCEL-EDITING command rolls back the changes present under an EDITABLE-COMPNENT and leaves editing"
  (assert (typep editable 'editable-component))
  (make-instance 'command-component
                 :icon (clone-icon 'cancel)
                 :visible (delay (edited-p editable))
                 :action (make-action (cancel-editing editable))))

(def (function e) make-store-editing-command (editable)
  "The STORE-EDITING command actually stores the changes present under an EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (make-instance 'command-component
                 :icon (make-icon-component 'store :label "Store" :tooltip "Store all changes")
                 :visible (delay (edited-p editable))
                 :action (make-action (store-editing editable)
                                      (add-user-message editable "Successfully stored changes"))))

(def (function e) make-revert-editing-command (editable)
  "The REVERT-EDITING command rolls back the changes present under an EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (make-instance 'command-component
                 :icon (make-icon-component 'revert :label "Revert" :tooltip "Revert all changes")
                 :visible (delay (edited-p editable))
                 :action (make-action (revert-editing editable))))

(def (function e) make-editing-commands (component)
  (list (make-begin-editing-command component)
        (make-save-editing-command component)
        (make-cancel-editing-command component)
        #+nil
        (make-store-command component)
        #+nil
        (make-revert-command component)))
