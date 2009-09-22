;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; resizable/mixin

(def (component e) resizable/mixin ()
  ((width
    :type number)
   (height
    :type number))
  (:documentation "A COMPONENT that remembers its size when resized at the remote side."))