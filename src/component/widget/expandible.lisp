;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Expandible

(def (component e) expandible (expandible/mixin)
  ())

;;;;;;
;;; Expandible abstract

(def (component e) expandible/abstract (expandible widget/abstract)
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
;;; Expandible widget

(def (component e) expandible/widget (widget/basic expandible/abstract)
  ())

(def (macro e) expandible/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'expandible/widget ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

(def render-component expandible/widget
  <span (:class "expandible")
    ,(render-component (make-toggle-expanded-command -self-))
    ,(if (expanded-component? -self-)
         (render-expanded-content-for -self-)
         (render-collapsed-content-for -self-))>)

;;;;;;
;;; Icon

(def (icon e) expand-component)

(def (icon e) collapse-component)

