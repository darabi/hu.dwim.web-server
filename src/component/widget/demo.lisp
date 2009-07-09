;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Demo widget

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
                      (make-instance 'lisp-form/viewer :component-value (make-lisp-form-component-value form)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "XHTML" :tooltip "Switch to the generated XHTML output"))
                      ;; TODO: make a component/xhtml-source/viewer
                      (quote-xml-string-content/widget ()
                        (render-to-xhtml-string component)))
                    (tab-page/widget (:selector (icon switch-to-tab-page :label "Component" :tooltip "Switch to the inspector of the live demo component"))
                      "TODO"
                      #+nil
                      (standard-object/inspector ()
                        component))))))

(def render-xhtml demo/widget
  <div (:class "demo widget")
    ,(render-content-for -self-)>)
