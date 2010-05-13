;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def (icon e) expand-component)

(def (icon e) collapse-component)

;;;;;;
;;; collapsible/component

(def (component e) collapsible/component (component/widget collapsible/mixin)
  ((collapse-command :type component)
   (expand-command :type component))
  (:documentation "A COLLAPSIBLE/COMPONENT has two different COMPONENTs as content, the EXPANDED and the COLLAPSED variants."))

(def refresh-component collapsible/component
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (collapse-command-of -self-)
          (make-collapse-command -self- class prototype value))
    (setf (expand-command-of -self-)
          (make-expand-command -self- class prototype value))))

(def method visible-child-component-slots ((self collapsible/component))
  (remove-slots (if (expanded-component? self)
                    '(expand-command)
                    '(collapse-command))
                (call-next-method)))

(def (function e) render-collapse-command-for (component)
  (render-component (collapse-command-of component)))

(def (function e) render-expand-command-for (component)
  (render-component (expand-command-of component)))

(def (function e) render-collapse-or-expand-command-for (component)
  (if (expanded-component? component)
      (render-collapse-command-for component)
      (render-expand-command-for component)))

(def (layered-function e) make-collapse-command (component class prototype value)
  (:method ((component collapsible/component) class prototype value)
    (command/widget (:subject-component component)
      (icon/widget collapse-component :label nil)
      (make-component-action component
        (collapse-component component)))))

(def (layered-function e) make-expand-command (component class prototype value)
  (:method ((component collapsible/component) class prototype value)
    (command/widget (:subject-component component)
      (icon/widget expand-component :label nil)
      (make-component-action component
        (expand-component component)))))

;;;;;;
;;; collapsible-content/component

(def (component e) collapsible-content/component (collapsible/component content/component)
  ())

(def method visible-child-component-slots ((self collapsible-content/component))
  (if (expanded-component? self)
      (call-next-method)
      (remove-slots '(content) (call-next-method))))

;;;;;;
;;; collapsible-contents/component

(def (component e) collapsible-contents/component (collapsible/component contents/component)
  ())

(def method visible-child-component-slots ((self collapsible-contents/component))
  (if (expanded-component? self)
      (call-next-method)
      (remove-slots '(contents) (call-next-method))))

;;;;;;
;;; collapsible/widget

(def (component e) collapsible/widget (standard/widget collapsible/component)
  ((collapsed-content :type component)
   (expanded-content :type component)))

(def (macro e) collapsible/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'collapsible/widget ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

(def render-component collapsible/widget
  (with-render-style/component (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-collapsed-or-expanded-content-for -self-)))

(def (function e) render-collapsed-content-for (component)
  (render-component (collapsed-content-of component)))

(def (function e) render-expanded-content-for (component)
  (render-component (expanded-content-of component)))

(def (function e) render-collapsed-or-expanded-content-for (component)
  (if (expanded-component? component)
      (render-expanded-content-for component)
      (render-collapsed-content-for component)))
