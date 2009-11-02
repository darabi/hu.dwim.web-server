;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Wizard

(def (component e) wizard/widget ()
  ((pages nil :type components)
   (page-navigation-bar nil :type component)))

(def (macro e) wizard/widget ((&rest args &key &allow-other-keys) &body pages)
  `(make-instance 'wizard/widget ,@args :pages (list ,@pages)))

(def constructor wizard/widget ()
  (bind (((:slots content pages page-navigation-bar) -self-))
    (setf page-navigation-bar (make-instance 'wizard-navigation-bar/widget
                                             :position 0
                                             :total-count (length pages)
                                             :page-size 1))))

(def render-xhtml wizard/widget
  (bind (((:read-only-slots pages page-navigation-bar) -self-))
    (render-vertical-list-layout (list (elt pages (position-of page-navigation-bar)) page-navigation-bar))))

(def layered-function finish-wizard (wizard))

(def layered-function cancel-wizard (wizard))

;;;;;;
;;; Wizard navigation bar

(def (component e) wizard-navigation-bar/widget (page-navigation-bar/widget)
  ((finish-command :type component)
   (cancel-command :type component)))

(def constructor wizard-navigation-bar/widget ()
  (bind (((:slots finish-command cancel-command) -self-))
    (setf finish-command (make-finish-wizard-command -self-)
          cancel-command (make-cancel-wizard-command -self-))))

(def render-xhtml wizard-navigation-bar/widget
  (bind (((:read-only-slots first-command previous-command next-command last-command finish-command cancel-command) -self-))
    (render-horizontal-list-layout (list first-command previous-command next-command last-command finish-command cancel-command))))

(def (function e) make-finish-wizard-command (wizard)
  (command/widget ()
    (icon finish-wizard)
    (make-component-action wizard
      (finish-wizard wizard))))

(def (function e) make-cancel-wizard-command (wizard)
  (command/widget ()
    (icon cancel-wizard)
    (make-component-action wizard
      (cancel-wizard wizard))))

(def (icon e) finish-wizard)

(def (icon e) cancel-wizard)
