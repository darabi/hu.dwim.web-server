;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Collapsible abstract

(def (component e) collapsible/abstract (widget/abstract collapsible/mixin)
  ((collapse-command :type component)
   (expand-command :type component))
  (:documentation "A COLLAPSIBLE/ABSTRACT has two different COMPONENTs as content, the EXPANDED and the COLLAPSED variants."))

(def refresh-component collapsible/abstract
  (bind ((class (component-dispatch-class -self-))
         (prototype (component-dispatch-prototype -self-))
         (value (component-value-of -self-)))
    (setf (collapse-command-of -self-)
          (make-collapse-command -self- class prototype value))
    (setf (expand-command-of -self-)
          (make-expand-command -self- class prototype value))))

(def method visible-child-component-slots ((self collapsible/abstract))
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
  (:method ((component collapsible/abstract) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon collapse-component :label nil)
      (make-component-action component
        (collapse-component component)))))

(def (layered-function e) make-expand-command (component class prototype value)
  (:method ((component collapsible/abstract) class prototype value)
    (command/widget (:ajax (ajax-of component))
      (icon expand-component :label nil)
      (make-component-action component
        (expand-component component)))))

;;;;;;
;;; Collapsible widget

(def (component e) collapsible/widget (widget/basic collapsible/abstract)
  ((collapsed-content :type component)
   (expanded-content :type component)))

(def (macro e) collapsible/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'collapsible/widget ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

(def render-component collapsible/widget
  <span (:class "collapsible")
    ,(render-collapse-or-expand-command-for -self-)
    ,(render-collapsed-or-expanded-content-for -self-)>)

(def (function e) render-collapsed-content-for (component)
  (render-component (collapsed-content-of component)))

(def (function e) render-expanded-content-for (component)
  (render-component (expanded-content-of component)))

(def (function e) render-collapsed-or-expanded-content-for (component)
  (if (expanded-component? component)
      (render-expanded-content-for component)
      (render-collapsed-content-for component)))

;;;;;;
;;; Icon

(def (icon e) expand-component)

(def (icon e) collapse-component)
