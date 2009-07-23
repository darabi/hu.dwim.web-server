;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; row/abstract

(def special-variable *row-index*)

(def (component e) row/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self row/abstract))
  #f)

;;;;;;
;;; rows/mixin

(def (component e) rows/mixin ()
  ((rows :type components))
  (:documentation "A COMPONENT with a SEQUENCE of ROWs."))

(def (function e) render-rows-for (component)
  (iter (for *row-index* :from 0)
        (for row :in-sequence (rows-of component))
        (render-component row)))
