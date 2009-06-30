;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Header mixin

(def (component e) header/mixin ()
  ((header :type component))
  (:documentation "A COMPONENT with a HEADER."))

(def (function e) render-header (component)
  (render-component (header-of component)))
