;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Wizard

(def (component e) wizard/widget (standard/widget)
  ((pages nil :type components)
   (page-navigation-bar nil :type component)))

(def (macro e) wizard/widget ((&rest args &key &allow-other-keys) &body pages)
  `(make-instance 'wizard/widget ,@args :pages (list ,@pages)))

(def constructor wizard/widget
  (bind (((:slots content pages page-navigation-bar) -self-))
    (setf page-navigation-bar (make-instance 'wizard-navigation-bar/widget
                                             :position 0
                                             :total-count (length pages)
                                             :page-size 1))))

(def render-xhtml wizard/widget
  (bind (((:read-only-slots pages page-navigation-bar) -self-))
    (render-vertical-list-layout (list (elt pages (position-of page-navigation-bar)) page-navigation-bar))))

(def layered-function finish-wizard (component class prototype value)
  (:method ((self wizard/widget) class prototype value)))

(def layered-function cancel-wizard (component class prototype value)
  (:method ((self wizard/widget) class prototype value)))

;;;;;;
;;; Wizard navigation bar

(def (component e) wizard-navigation-bar/widget (page-navigation-bar/widget)
  ((finish-command :type component)
   (cancel-command :type component)))

(def refresh-component wizard-navigation-bar/widget
  (bind (((:slots finish-command cancel-command) -self-)
         (component (parent-component-of -self-))
         (class (component-dispatch-class component))
         (prototype (component-dispatch-prototype component))
         (value (component-value-of component)))
    (setf finish-command (make-finish-wizard-command component class prototype value)
          cancel-command (make-cancel-wizard-command component class prototype value))))

(def render-xhtml wizard-navigation-bar/widget
  (bind (((:read-only-slots first-command previous-command next-command last-command finish-command cancel-command) -self-))
    (render-horizontal-list-layout (list first-command previous-command next-command last-command finish-command cancel-command))))

(def (layered-function e) make-finish-wizard-command (component class prototype value)
  (:method ((component wizard/widget) class prototype value)
    (command/widget ()
      (icon/widget finish-wizard)
      (make-component-action component
        (finish-wizard component class prototype value)))))

(def (layered-function e) make-cancel-wizard-command (component class prototype value)
  (:method ((component wizard/widget) class prototype value)
    (command/widget ()
      (icon/widget cancel-wizard)
      (make-component-action component
        (cancel-wizard component class prototype value)))))

(def (icon e) finish-wizard)

(def (icon e) cancel-wizard)
