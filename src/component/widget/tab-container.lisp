;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Tab container component

(def (component e) tab-container-component (content/mixin)
  ((pages nil) ;; NOTE: this should not be a component slot, because components are switched from here to the content
   (command-bar nil :type component)))

(def (macro e) tab-container (&body pages)
  `(make-instance 'tab-container-component :pages (remove-if #'null (list ,@pages))))

(def render-xhtml tab-container-component
  (bind (((:read-only-slots content command-bar) -self-))
    <div (:class "tab-container")
         ,(render-vertical-list (list command-bar content))>))

(def refresh-component tab-container-component
  (bind (((:slots pages command-bar content) -self-))
    (setf command-bar
          (make-instance 'command-bar/basic
                         :commands (mapcar (lambda (page)
                                             (make-switch-to-tab-page-command -self- page))
                                           pages)))
    (unless content
      (setf content (first pages)))))

(def function make-switch-to-tab-page-command (tab-container tab-page)
  (make-replace-command (delay (content-of tab-container))
                        tab-page
                        :content (header-of tab-page)
                        :ajax (ajax-id tab-container)))

(def (icon e) swith-to-page)

;;;;;;
;;; Tab page

(def (component e) tab-page-component (content/mixin)
  ((header nil :type component)))

(def (macro e) tab-page (header &body forms)
  `(make-instance 'tab-page-component :header ,header :content (progn ,@forms)))
