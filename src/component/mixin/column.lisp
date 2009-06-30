;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Columns mixin

(def (component e) columns/mixin ()
  ((columns :type components))
  (:documentation "A COMPONENT with a SEQUENCE of COLUMNs."))

(def (function e) render-columns (component)
  (foreach #'render-component (columns-of component)))

;;;;;;
;;; Column headers mixin

(def (component e) column-headers/mixin ()
  ((column-headers :type components))
  (:documentation "A COMPONENT with a SEQUENCE of COLUMN-HEADERs."))

(def (function e) render-column-headers (component)
  (foreach #'render-component (column-headers-of component)))
