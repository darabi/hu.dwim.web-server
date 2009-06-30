;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Resizable mixin

(def (component e) resizable/mixin ()
  ((width
    :type number)
   (height
    :type number))
  (:documentation "A COMPONENT that remembers its size when resized at the remote side."))
