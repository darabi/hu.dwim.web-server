;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Editable mixin

(def (component e) editable/mixin ()
  ((edited-component
    #f
    :type boolean
    :initarg :edited
    :documentation "TRUE means COMPONENT is currently being edited, FALSE otherwise."))
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

(def refresh-component editable/mixin
  (if (edited-component? -self-)
      (join-editing -self-)
      (leave-editing -self-)))

(def method editable-component? ((self editable/mixin))
  #t)

(def method begin-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (join-editing self))

(def method save-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (store-editing self)
  (leave-editing self))

(def method cancel-editing ((self editable/mixin))
  (declare (optimize (debug 2))) ;; we always want to see it in backtraces
  (revert-editing self)
  (leave-editing self))

(def method join-editing :before ((self editable/mixin))
  (setf (edited-component? self) #t))

(def method leave-editing :before ((self editable/mixin))
  (setf (edited-component? self) #f))
