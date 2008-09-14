;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Abstract menu item

(def component abstract-menu-item-component ()
  ((menu-items nil :type components)))

;;;;;;
;;; Menu

(def component menu-component (abstract-menu-item-component)
  ((target-place nil :type place :export :accessor)
   (icon nil :type component)))

(def (function e) make-menu-component (label menu-items)
  (bind ((menu-items (iter (for menu-item :in menu-items)
                           (if (listp menu-item)
                               (appending menu-item)
                               (collect menu-item)))))
    (when menu-items
      (make-instance 'menu-component
                     :icon (when label
                             (icon menu :label label :tooltip nil))
                     :menu-items menu-items))))

(def (macro e) menu (label &body menu-items)
  `(make-menu-component ,label (list ,@menu-items)))

(def function render-menu-items (menu-items)
  (mapcar #'render menu-items))

(def render menu-component ()
  (bind (((:read-only-slots icon menu-items) -self-))
    <div ,(if icon
              (render icon)
              +void+)
         <ul ,@(mapcar #'render menu-items)>>))

(def icon menu "static/wui/icons/20x20/open-folder.png") ;; TODO: icon

;;;;;;
;;; Menu item

(def component menu-item-component (abstract-menu-item-component)
  ((command nil :type component)))

(def (function e) make-menu-item-component (command &rest menu-items)
  (bind ((menu-items (remove nil menu-items)))
    (when (or menu-items
              (typep command 'command-component))
      (make-instance 'menu-item-component :command command :menu-items menu-items))))

(def (macro e) menu-item (command &body menu-items)
  `(make-menu-item-component ,command ,@menu-items))

(def render menu-item-component ()
  (with-slots (command menu-items) -self-
    <div ,(render command)
         <ul ,@(mapcar #'render menu-items)>>))

;;;;;;
;;; Replace menu target command

(def component replace-menu-target-command-component (command-component)
  ((component)))

(def constructor replace-menu-target-command-component ()
  (setf (action-of -self-)
        (make-action
          (bind ((menu-component
                  (find-ancestor-component -self-
                                           (lambda (ancestor)
                                             (and (typep ancestor 'menu-component)
                                                  (target-place-of ancestor)))))
                 (component (force (component-of -self-))))
            (setf (component-of -self-) component
                  (component-at-place (target-place-of menu-component)) component)))))

(def (macro e) replace-menu-target-command (icon &body forms)
  `(make-instance 'replace-menu-target-command-component :icon ,icon :component (delay ,@forms)))

;;;;;;
;;; Debug menu

(def (function e) make-debug-menu ()
  (menu "Debug"
    (menu-item (command "Start over"
                        (make-action (reset-frame-root-component))))
    (menu-item (command "Toggle test mode"
                        (make-action (notf (running-in-test-mode-p *application*)))))
    (menu-item (command "Toggle profiling"
                        (make-action (notf (profile-request-processing-p *server*)))))
    (menu-item (command "Toggle hierarchy"
                        (make-action (toggle-debug-component-hierarchy *frame*))))
    (menu-item (command "Toggle debug client side"
                        (make-action (notf (debug-client-side? (root-component-of *frame*))))))
    #+sbcl
    (menu-item (replace-menu-target-command "Frame size breakdown"
                 (make-instance 'frame-size-breakdown-component)))))
