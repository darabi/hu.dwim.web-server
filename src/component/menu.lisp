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
  (bind ((menu-items (remove nil menu-items)))
    (make-instance 'menu-component
                   :icon (when label
                           (clone-icon 'menu :label label :tooltip nil))
                   :menu-items menu-items)))

(def (macro e) menu (label &body menu-items)
  `(make-menu-component ,label (list ,@menu-items)))

(def function render-menu-items (menu-items)
  (mapcar #'render ))

(def render menu-component ()
  (bind ((icon (icon-of -self-)))
    <div ,(if icon
              (render icon)
              +void+)
         <ul ,@(mapcar #'render (menu-items-of -self-))>>))

(def icon menu "static/wui/icons/20x20/file-download.png") ;; TODO: icon

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
  (with-slots (visible command menu-items) -self-
    <div ,(render command)
         <ul ,@(mapcar #'render menu-items)>>))

;;;;;;
;;; Replace menu target command

(def component replace-menu-target-command-component (command-component)
  ((action :type action)
   (component)))

(def constructor replace-menu-target-command-component ()
  (with-slots (action component) -self-
    (setf action
          (make-action
            (bind ((menu-component
                    (find-ancestor-component -self-
                                             (lambda (ancestor)
                                               (and (typep ancestor 'menu-component)
                                                    (target-place-of ancestor))))))
              (setf (component-at-place (target-place-of menu-component)) (force component)))))))

(def (macro e) replace-menu-target-command (icon component)
  `(make-instance 'replace-menu-target-command-component :icon ,icon :component (delay ,component)))

(def method cl-quasi-quote::collect-slots-for-syntax-node-emitting-form ((node replace-menu-target-command-component))
  (remove 'action (call-next-method) :key #'slot-definition-name))

;;;;;;
;;; Standard object filter menu item

(def component standard-object-filter-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor standard-object-filter-menu-item-component ()
  (with-slots (the-class command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (clone-icon 'filter)
                                 :component (make-filter-component the-class)))))

(def (function e) make-standard-object-filter-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::filter-entity-operation :-entity- class)
      (make-instance 'standard-object-filter-menu-item-component :the-class class))))

(def (macro e) standard-object-filter-menu-item (class-name)
  `(make-standard-object-filter-menu-item-component ,class-name))

;;;;;;
;;; Standard object maker menu item

(def component standard-object-maker-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor standard-object-maker-menu-item-component ()
  (with-slots (the-class visible command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (clone-icon 'new)
                                 :component (make-maker-component the-class)))))

(def (function e) make-standard-object-maker-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::create-entity-operation :-entity- class)
      (make-instance 'standard-object-maker-menu-item-component :the-class class))))

(def (macro e) standard-object-maker-menu-item (class-name)
  `(make-standard-object-maker-menu-item-component ,class-name))

;;;;;;
;;; Persistent process starter menu item

(def component persistent-process-starter-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor persistent-process-starter-menu-item-component ()
  (with-slots (the-class visible command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (clone-icon 'new)
                                 :component (make-maker-component the-class)))))

(def (function e) make-persistent-process-starter-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::start-persistent-process-operation :-entity- class)
      (make-instance 'persistent-process-starter-menu-item-component :the-class class))))

(def (macro e) persistent-process-starter-menu-item (class-name)
  `(make-persistent-process-starter-menu-item-component ,class-name))

