;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Cells mixin

(def (component e) cells/mixin ()
  ((cells :type components))
  (:documentation "A COMPONENT with a SEQUENCE of CELLs."))

(def (function e) render-cells (component)
  (foreach #'render-cell (cells-of component)))

(def (layered-function e) render-cell (component)
  (:method :in xhtml-layer ((self number))
    <td ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <td ,(render-component self)>)

  (:method ((self component))
    (render-component self)))
