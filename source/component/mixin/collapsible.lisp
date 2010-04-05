;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; collapsible/mixin

(def (component e) collapsible/mixin ()
  ((collapsible-component
    #t
    :type boolean
    :initarg :collapsible
    :computed-in computed-universe/session
    :documentation "TRUE means COMPONENT can be EXPANDED/COLLAPSED, FALSE otherwise.")
   (expanded-component
    #t
    :type boolean
    :initarg :expanded
    :computed-in computed-universe/session
    :documentation "TRUE means COMPONENT displays itself with full detail, FALSE means it should be minimized."))
  (:documentation "A COMPONENT that can be EXPANDED or COLLAPSED."))

(def method expand-component ((self collapsible/mixin))
  (setf (expanded-component? self) #t))

(def method collapse-component ((self collapsible/mixin))
  (setf (expanded-component? self) #f))

