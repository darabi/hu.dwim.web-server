;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; row/component

(def special-variable *row-index* nil)

(def (component e) row/component ()
  ())

(def component-environment row/component
  (if *row-index*
      (call-next-method)
      (bind ((*row-index* (awhen (parent-component-of -self-)
                            (position -self- (rows-of it)))))
        (call-next-method))))

(def method supports-debug-component-hierarchy? ((self row/component))
  #f)

;;;;;;
;;; rows/mixin

(def (component e) rows/mixin ()
  ((rows nil :type components))
  (:documentation "A COMPONENT with a SEQUENCE of ROWs."))

(def component-environment rows/mixin
  (bind ((*row-index* nil))
    (call-next-method)))

(def (function e) render-rows-for (component)
  (iter (for *row-index* :from 0)
        (for row :in-sequence (rows-of component))
        (render-component row)))
