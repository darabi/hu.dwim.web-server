;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; row/abstract

(def special-variable *row-index* nil)

(def (component e) row/abstract ()
  ())

(def component-environment row/abstract
  (if *row-index*
      (call-next-method)
      (bind ((*row-index* (awhen (parent-component-of -self-)
                            (position -self- (rows-of it)))))
        (call-next-method))))

(def method supports-debug-component-hierarchy? ((self row/abstract))
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
