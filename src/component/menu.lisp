;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Menu

(def component menu-component ()
  ((target-place :type place :export :accessor)
   (menu-items nil :type components)))

(def (macro e) menu (&body menu-items)
  `(make-instance 'menu-component :menu-items (optional-list ,@menu-items)))

(def render menu-component ()
  (with-slots (menu-items) -self-
    <ul ,@(mapcar #'render menu-items)>))

;;;;;;
;;; Menu item

(def component menu-item-component ()
  ((command :type component)
   (menu-items nil :type components)))

(def (macro e) menu-item (command &body menu-items)
  `(make-instance 'menu-item-component :command ,command :menu-items (optional-list ,@menu-items)))

(def render menu-item-component ()
  (with-slots (visible command menu-items) -self-
    <li ,(render command)
        <ul ,@(mapcar #'render menu-items)>>))

;;;;;;
;;; Replace menu target command

(def component replace-menu-target-command-component (command-component)
  ((action)
   (component)))

(def constructor replace-menu-target-command-component ()
  (with-slots (action component) -self-
    (setf action
          (make-action
            (bind ((menu-component (find-ancestor-component-with-type -self- 'menu-component)))
              (setf (component-at-place (target-place-of menu-component)) component))))))

(def (macro e) replace-menu-target-command (icon component)
  `(make-instance 'replace-menu-target-command-component :icon ,icon :component ,component))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node replace-menu-target-command-component))
  (remove 'action (call-next-method) :key #'slot-definition-name))

;;;;;;
;;; Standard object filter menu item

(def component standard-object-filter-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor standard-object-filter-menu-item-component ()
  (with-slots (the-class command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (make-icon-component 'filter :label "Keresés")
                                 :component (make-filter-component the-class)))))

(def (function e) make-standard-object-filter-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::filter-entity-operation :-entity- class)
      (make-instance 'standard-object-filter-menu-item-component :the-class class))))

;;;;;;
;;; Standard object maker menu item

(def component standard-object-maker-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor standard-object-maker-menu-item-component ()
  (with-slots (the-class visible command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (make-icon-component 'new :label "Új")
                                 :component (make-maker-component the-class)))))

(def (function e) make-standard-object-maker-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::create-entity-operation :-entity- class)
      (make-instance 'standard-object-maker-menu-item-component :the-class class))))
