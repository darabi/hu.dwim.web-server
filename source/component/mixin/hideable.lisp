;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; hideable/mixin

(def (component e) hideable/mixin ()
  ((hideable-component
    #t
    :type boolean
    :initarg :hideable
    :computed-in compute-as
    :documentation "TRUE means COMPONENT can be VISIBLE/HIDDEN, FALSE otherwise.")
   (visible-component
    #t
    :type boolean
    :initarg :visible
    :computed-in compute-as
    :documentation "TRUE means the COMPONENT is visible on the remote side, FALSE otherwise."))
  (:documentation "A COMPONENT that can be HIDDEN or SHOWN."))

(def render-component :around hideable/mixin
  (when (visible-component? -self-)
    (call-next-method)))

(def method visible-component? :around ((self hideable/mixin))
  (force (call-next-method)))

(def method hide-component ((self hideable/mixin))
  (setf (visible-component? self) #f))

(def method show-component ((self hideable/mixin))
  (setf (visible-component? self) #t))
