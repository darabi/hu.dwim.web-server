;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Standard object filter menu item

(def component standard-object-filter-menu-item-component (menu-item-component)
  ((the-class)))

(def constructor standard-object-filter-menu-item-component ()
  (with-slots (the-class command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (icon filter)
                                 :component (make-filter the-class)))))

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
  (with-slots (the-class command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (icon new)
                                 :component (make-maker the-class)))))

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
  (with-slots (the-class command) -self-
    (setf command (make-instance 'replace-menu-target-command-component
                                 :icon (icon new)
                                 :component (make-maker the-class)))))

(def (function e) make-persistent-process-starter-menu-item-component (class-name)
  (bind ((class (find-class class-name)))
    (when (dmm::authorize-operation 'dmm::start-persistent-process-operation :-entity- class)
      (make-instance 'persistent-process-starter-menu-item-component :the-class class))))

(def (macro e) persistent-process-starter-menu-item (class-name)
  `(make-persistent-process-starter-menu-item-component ,class-name))