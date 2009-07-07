;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Collapsible

(def (component e) collapsible (collapsible/mixin)
  ())

;;;;;;
;;; Collapsible abstract

(def (component e) collapsible/abstract (collapsible widget/abstract)
  ((collapsed-content :type component)
   (expanded-content :type component)
   (toggle-command :type component))
  (:documentation "A COMPONENT with two different COMPONENTs as content, the expanded and the collapsed variants."))

(def function render-collapsed-content-for (component)
  (render-component (collapsed-content-of component)))

(def function render-expanded-content-for (component)
  (render-component (expanded-content-of component)))

;; TODO: move
(def function make-toggle-expanded-command (component)
  (command/widget (:ajax (ajax-of component))
    (if (expanded-component? component)
        (icon collapse-component)
        (icon expand-component))
    (make-component-action component
      (if (expanded-component? component)
          (collapse-component component)
          (expand-component component)))))

;;;;;;
;;; Collapsible widget

(def (component e) collapsible/widget (widget/basic collapsible/abstract)
  ())

(def (macro e) collapsible/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'collapsible/widget ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

(def render-component collapsible/widget
  <span (:class "collapsible")
    ,(render-component (make-toggle-expanded-command -self-))
    ,(if (expanded-component? -self-)
         (render-expanded-content-for -self-)
         (render-collapsed-content-for -self-))>)

;;;;;;
;;; Icon

(def (icon e) expand-component)

(def (icon e) collapse-component)

