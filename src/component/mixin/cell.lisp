;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;
;;; Header cell mixin

(def (component e) header-cell/mixin ()
  ((header-cell :type component))
  (:documentation "A COMPONENT with a HEADER-CELL."))

;;;;;;
;;; Cells mixin

(def (component e) cells/mixin ()
  ((cells :type components))
  (:documentation "A COMPONENT with a SEQUENCE of CELLs."))

(def (function e) render-cells-for (component)
  (iter (for *column-index* :from 0)
        (for cell :in-sequence (cells-of component))
        (render-cell cell)))

(def (layered-function e) render-cell (component)
  (:method :in xhtml-layer ((self number))
    <td ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <td ,(render-component self)>)

  (:method ((self component))
    (render-component self)))