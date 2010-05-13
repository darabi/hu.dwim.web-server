;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; lisp-form/component-demo/inspector

(def (component e) lisp-form/component-demo/inspector (t/inspector content/component)
  ((component :type function)))

(def (macro e) lisp-form/component-demo/inspector ((&rest args &key &allow-other-keys) &body forms)
  `(make-instance 'lisp-form/component-demo/inspector ,@args :component-value ',(the-only-element forms) :component (delay ,@forms)))

(def refresh-component lisp-form/component-demo/inspector
  (bind (((:slots component component-value content) -self-)
         (component (force component)))
    (setf content (tab-container/widget ()
                    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Demo" :tooltip "Running live demo component"))
                      component)
                    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Source" :tooltip "Original lisp source code that was used to create the component"))
                      (make-instance 't/lisp-form/inspector :component-value (make-lisp-form-component-value component-value)))
                    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "XHTML" :tooltip "Generated XHTML output"))
                      (make-value-inspector component :initial-alternative-type 'component/render-xhtml-output/inspector))
                    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Component" :tooltip "Live demo component internal state inspector"))
                      (make-value-inspector component :initial-alternative-type 't/name-value-list/inspector))
                    (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "Documentation" :tooltip "Component class documentation"))
                      (make-value-inspector component :initial-alternative-type 't/documentation/inspector))))))

(def render-xhtml lisp-form/component-demo/inspector
  (with-render-style/component (-self-)
    (render-content-for -self-)))
