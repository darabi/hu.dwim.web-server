;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu

(def component menu-component ()
  ((target-place :type place)
   (menu-items nil :type components)))

(def render menu-component ()
  (with-slots (menu-items) self
    <ul ,@(mapcar #'render menu-items)>))

(def component menu-item-component ()
  ((command :type component)
   (menu-items nil :type components)))

(def render menu-item-component ()
  (with-slots (command menu-items) self
    <li ,(render command)
        <ul ,@(mapcar #'render menu-items)>>))

(def component replace-menu-target-command-component (command-component)
  ((action)
   (component)))

(def constructor replace-menu-target-command-component ()
  (with-slots (action component) self
    (setf action
          (make-action
            (bind ((menu-component (find-ancestor-component-with-type self 'menu-component)))
              (setf (component-at-place (target-place-of menu-component)) component))))))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node replace-menu-target-command-component))
  (remove 'action (call-next-method) :key #'slot-definition-name))
