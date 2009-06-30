;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Footer

(def (component e) footer/mixin ()
  ((footer :type component))
  (:documentation "A COMPONENT with a FOOTER."))

(def (function e) render-footer (component)
  (render-component (footer-of component)))
