;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Rows mixin

(def (component e) rows/mixin ()
  ((rows :type components))
  (:documentation "A COMPONENT with a SEQUENCE of ROWs."))

(def (function e) render-rows-for (component)
  (foreach #'render-component (rows-of component)))

;;;;;;
;;; Row headers mixin

(def (component e) row-headers/mixin ()
  ((row-headers :type components))
  (:documentation "A COMPONENT with a SEQUENCE of ROW-HEADERs."))

(def (function e) render-row-headers-for (component)
  (foreach #'render-component (row-headers-of component)))
