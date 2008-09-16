;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Wizard

(def component wizard-component ()
  ((pages nil :type components)
   (page-navigation-bar nil :type component)))

(def constructor wizard-component ()
  (with-slots (content pages page-navigation-bar) -self-
    (setf page-navigation-bar (make-instance 'wizard-navigation-bar-component
                                             :position 0
                                             :total-count (length pages)
                                             :page-count 1))))

(def render wizard-component ()
  (bind (((:read-only-slots pages page-navigation-bar) -self-))
    (render-vertical-list (list (elt pages (position-of page-navigation-bar)) page-navigation-bar))))

(def generic finish-wizard (wizard))

(def generic cancel-wizard (wizard))

;;;;;;
;;; Wizard navigation bar

(def component wizard-navigation-bar-component (page-navigation-bar-component)
  ((finish-command :type component)
   (cancel-command :type component)))

(def constructor wizard-navigation-bar-component ()
  (with-slots (finish-command cancel-command) -self-
    (setf finish-command (make-finish-wizard-command -self-)
          cancel-command (make-cancel-wizard-command -self-))))

(def render wizard-navigation-bar-component ()
  (bind (((:read-only-slots first-command previous-command next-command last-command finish-command cancel-command) -self-))
    (render-horizontal-list (list first-command previous-command next-command last-command finish-command cancel-command))))

(def (function e) make-finish-wizard-command (wizard)
  (make-instance 'command-component
                 :icon (icon finish)
                 :action (make-action (finish-wizard wizard))))

(def (function e) make-cancel-wizard-command (wizard)
  (make-instance 'command-component
                 :icon (icon cancel)
                 :action (make-action (cancel-wizard wizard))))
