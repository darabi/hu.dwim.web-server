;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; demo/widget

;; TODO: rename this to lisp-form/component-demo/viewer and move
(def (component e) demo/widget (widget/basic content/abstract)
  ((form :type t)))

(def (macro e) demo/widget ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'demo/widget ,@args :form ',(the-only-element forms)))

(def refresh-component demo/widget
  (bind (((:slots form content) -self-)
         (component (eval form)))
    (setf content (tab-container/widget ()
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Demo" :tooltip "Switch to the live demo component"))
                      component)
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Source" :tooltip "Switch to the original lisp source"))
                      (make-instance 't/lisp-form/inspector :component-value (make-lisp-form-component-value form)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "XHTML" :tooltip "Switch to the generated XHTML output"))
                      ;; TODO: make a component/xhtml-source/viewer
                      (quote-xml-string-content/widget ()
                        (render-to-xhtml-string component)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Component" :tooltip "Switch to the inspector of the live demo component"))
                      (make-value-inspector component))))))

(def render-xhtml demo/widget
  <div (:class "demo widget")
    ,(render-content-for -self-)>)
