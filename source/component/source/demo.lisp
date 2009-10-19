;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lisp-form/component-demo/inspector

(def (component e) lisp-form/component-demo/inspector (inspector/style content/abstract)
  ((component :type function)))

(def (macro e) lisp-form/component-demo/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'lisp-form/component-demo/inspector ,@args :component-value ',(the-only-element forms) :component (delay ,@forms)))

(def refresh-component lisp-form/component-demo/inspector
  (bind (((:slots component component-value content) -self-)
         (component (force component)))
    (setf content (tab-container/widget ()
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Demo" :tooltip "Switch to the live demo component"))
                      component)
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Source" :tooltip "Switch to the original lisp source"))
                      (make-instance 't/lisp-form/inspector :component-value (make-lisp-form-component-value component-value)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "XHTML" :tooltip "Switch to the generated XHTML output"))
                      ;; TODO: make a component/xhtml-source/inspector
                      (quote-xml-string-content/widget ()
                        (render-to-xhtml-string component)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Component" :tooltip "Switch to the inspector of the live demo component"))
                      (make-inspector (class-of component) component))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Documentation" :tooltip "Switch to the documentation of the component class"))
                      (component-documentation component))))))

(def render-xhtml lisp-form/component-demo/inspector
  (with-render-style/abstract (-self-)
    (render-content-for -self-)))
