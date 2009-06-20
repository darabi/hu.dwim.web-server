;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Expandible abstract

(def (component e) expandible/abstract ()
  ((collapsed-content :type component)
   (expanded-content :type component))
  (:documentation "A component with two different contents, the expanded and the collapsed variants."))

(def (macro e) expandible ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'expandible/basic ,@args :collapsed-content ,(first content) :expanded-content ,(second content)))

;;;;;;
;;; Expandible abstract

(def (component e) expandible/basic (expandible/abstract expandible/mixin)
  ())

(def render-component expandible/basic
  <span ,(render-expandible-handle -self-)
    ,(if (expanded? -self-)
         (render-component (expanded-content-of -self-))
         (render-component (collapsed-content-of -self-)))>)

;;;;;;
;;; Expandible full

(def (component e) expandible/full (expandible/basic content/basic component/full)
  ())

(def render-component expandible/full
  (with-render-style/abstract (-self-)
    (call-next-method)))
