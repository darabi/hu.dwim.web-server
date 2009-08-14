;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; disableable/mixin

(def (component e) disableable/mixin ()
  ((enabled-component
    #t
    :type boolean
    :initarg :enabled
    :computed-in compute-as
    :documentation "TRUE means COMPONENT is ENABLED on the remote side, FALSE otherwise."))
  (:documentation "A COMPONENT that can be ENABLED or DISABLED."))

(def method enable-component ((self disableable/mixin))
  (setf (enabled-component? self) #t))

(def method disable-component ((self disableable/mixin))
  (setf (enabled-component? self) #f))
