;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Expandible

(def (component e) expandible (expandible/mixin)
  ())

(def (macro e) expandible ((&rest args &key &allow-other-keys) &body content)
  `(expandible/basic ,args ,@content))

;;;;;;
;;; Expandible abstract

(def (component e) expandible/abstract (expandible component/abstract)
  ((collapsed-content :type component)
   (expanded-content :type component)
   (toggle-command :type component))
  (:documentation "A COMPONENT with two different COMPONENTs as content, the expanded and the collapsed variants."))

;; TODO: move
(def function make-toggle-expanded-command (component)
  (command (:ajax (ajax-of component))
    (if (expanded-component? component)
        (icon collapse-component)
        (icon expand-component))
    (make-component-action component
      (if (expanded-component? component)
          (collapse-component component)
          (expand-component component)))))

;;;;;;
;;; Expandible abstract

(def (component e) expandible/basic (expandible/abstract component/basic)
  ())

(def (macro e) expandible/basic ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'expandible/basic ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

(def render-component expandible/basic
  <span (:class "expandible")
    ,(render-component (make-toggle-expanded-command -self-))
    ,(if (expanded-component? -self-)
         (render-component (expanded-content-of -self-))
         (render-component (collapsed-content-of -self-)))>)

;;;;;;
;;; Expandible full

(def (component e) expandible/full (expandible/basic component/full)
  ())

(def render-component expandible/full
  (with-render-style/abstract (-self-)
    (call-next-method)))
