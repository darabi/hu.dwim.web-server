;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Wizard

(def (component e) wizard-component ()
  ((pages nil :type components)
   (page-navigation-bar nil :type component)))

(def constructor wizard-component ()
  (bind (((:slots content pages page-navigation-bar) -self-))
    (setf page-navigation-bar (make-instance 'wizard-navigation-bar-component
                                             :position 0
                                             :total-count (length pages)
                                             :page-size 1))))

(def render-xhtml wizard-component
  (bind (((:read-only-slots pages page-navigation-bar) -self-))
    (render-vertical-list (list (elt pages (position-of page-navigation-bar)) page-navigation-bar))))

(def generic finish-wizard (wizard))

(def generic cancel-wizard (wizard))

;;;;;;
;;; Wizard navigation bar

(def (component e) wizard-navigation-bar-component (page-navigation-bar-component)
  ((finish-command :type component)
   (cancel-command :type component)))

(def constructor wizard-navigation-bar-component ()
  (bind (((:slots finish-command cancel-command) -self-))
    (setf finish-command (make-finish-wizard-command -self-)
          cancel-command (make-cancel-wizard-command -self-))))

(def render-xhtml wizard-navigation-bar-component
  (bind (((:read-only-slots first-command previous-command next-command last-command finish-command cancel-command) -self-))
    (render-horizontal-list-layout (list first-command previous-command next-command last-command finish-command cancel-command))))

(def (function e) make-finish-wizard-command (wizard)
  (command ()
    (icon finish)
    (make-component-action wizard
      (finish-wizard wizard))))

(def (function e) make-cancel-wizard-command (wizard)
  (command ()
    (icon cancel)
    (make-component-action wizard
      (cancel-wizard wizard))))
