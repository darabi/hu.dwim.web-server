;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tab container component

(def component tab-container-component (content-component)
  ((icons nil :type components)
   (pages nil :type components)
   (command-bar nil :type component)))

(def constructor tab-container-component ()
  (with-slots (icons pages command-bar) -self-
    (setf command-bar
          (make-instance 'command-bar-component
                         :commands (mapcar (lambda (icon page)
                                             (make-switch-to-tab-page-command -self- icon page))
                                           icons pages)))))

(def (macro e) tab-container (&body icon-page-pairs)
  `(make-instance 'tab-container-component
                  :icons (list ,@(mapcar #'first icon-page-pairs))
                  :pages (list ,@(mapcar #'second icon-page-pairs))))

(def render tab-container-component ()
  (with-slots (content command-bar) -self-
    (render-vertical-list (list command-bar content))))

(def method refresh-component ((self tab-container-component))
  (with-slots (pages content) self
    (unless content
      (setf content (first pages)))))

(def function make-switch-to-tab-page-command (tab-container icon tab-page)
  (make-replace-command (delay (content-of tab-container)) tab-page :icon icon))

(def icon swith-to-page "static/wui/icons/20x20/eye.png")
(defresources hu
  (icon-label.swith-to-page "Lap")
  (icon-tooltip.swith-to-page "A lap előrehozása"))
(defresources en
  (icon-label.swith-to-page "Page")
  (icon-tooltip.swith-to-page "Switch to page"))
