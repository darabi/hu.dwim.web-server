;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Border mixin

(def (component e) border/mixin ()
  ((border :type component))
  (:documentation "A COMPONENT with a BORDER."))
