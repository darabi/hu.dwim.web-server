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
  ;; TODO: reuse the flags slot
  ((edited #f :type boolean :documentation "TRUE indicates the component is currently being edited, FALSE otherwise.")))

(def (function e) begin-editing (editable)
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (assert (typep editable 'editable-component))
  (join-editing editable))

(def (function e) save-editing (editable &key (leave-editing #t))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (assert (typep editable 'editable-component))
  (catch 'abort-save-editing
    (store-editing editable)
    (when leave-editing
      (leave-editing editable))))

(def (function e) abort-save-editing ()
  (throw 'abort-save-editing #t))

(def (function e) cancel-editing (editable)
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (assert (typep editable 'editable-component))
  (revert-editing editable)
  (leave-editing editable))

(def generic map-editable-child-components (component function)
  (:method ((component component) function)
    (ensure-functionf function)
    (map-child-components component (lambda (child)
                                      (when (typep child 'editable-component)
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
  (find-editable-child-component component 'edited-p))

(def function has-edited-descendant-component-p (component)
  (find-editable-descendant-component component 'edited-p))

;;;;;;
;;; Customization points

(def (generic e) join-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'join-editing))

  (:method :before ((component editable-component))
    (setf (edited-p component) #t))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def (generic e) leave-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'leave-editing))

  (:method :before ((component editable-component))
    (setf (edited-p component) #f))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def (generic e) store-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'store-editing))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def (generic e) revert-editing (component)
  (:method ((component component))
    (map-editable-child-components component 'revert-editing))

  (:method :around ((component component))
    (call-in-component-environment component #'call-next-method)))

(def method refresh-component :after ((self editable-component))
  (if (edited-p self)
      (join-editing self)
      (leave-editing self)))

;;;;;;
;;; Commands

(def (function e) make-begin-editing-command (editable &key visible)
  "The BEGIN-EDITING command starts editing underneath the given EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (command (icon edit)
           (make-component-action editable
             (begin-editing editable))
           :visible (or visible (delay (not (edited-p editable))))))

(def (function e) make-save-editing-command (editable)
  "The SAVE-EDITING command actually makes the changes present under an EDITABLE-COMPNENT and leaves editing"
  (assert (typep editable 'editable-component))
  (command (icon save)
           (make-component-action editable
             (save-editing editable))
           :visible (delay (edited-p editable))))

(def (function e) make-cancel-editing-command (editable)
  "The CANCEL-EDITING command rolls back the changes present under an EDITABLE-COMPNENT and leaves editing"
  (assert (typep editable 'editable-component))
  (command (icon cancel-editing)
           (make-component-action editable
             (cancel-editing editable))
           :visible (delay (edited-p editable))))

(def (function e) make-store-editing-command (editable)
  "The STORE-EDITING command actually stores the changes present under an EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (command (icon store)
           (make-component-action editable
             (save-editing editable :leave-editing #f))
           :visible (delay (edited-p editable))))

(def (function e) make-revert-editing-command (editable)
  "The REVERT-EDITING command rolls back the changes present under an EDITABLE-COMPNENT"
  (assert (typep editable 'editable-component))
  (command (icon revert)
           (make-component-action editable
             (revert-editing editable))
           :visible (delay (edited-p editable))))

(def (layered-function e) make-editing-commands (component class instance-or-prototype)
  (:method ((component component) (class standard-class) (instance-or-prototype standard-object))
    (if (inherited-initarg component :store-mode)
        (list (make-store-editing-command component)
              (make-revert-editing-command component))
        (list (make-begin-editing-command component)
              (make-save-editing-command component)
              (make-cancel-editing-command component)))))

(def layered-method make-standard-commands ((component editable-component) (class standard-class) (instance-or-prototype standard-object))
  (append (make-editing-commands component class instance-or-prototype) (call-next-method)))

(def layered-method make-refresh-command ((component editable-component) (class standard-class) (prototype-or-instance standard-object))
  (command (icon refresh)
           (make-component-action component
             (refresh-component component))
           :visible (delay (not (edited-p component)))))
