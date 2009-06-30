;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Border widget

(def (component e) border/widget (widget/basic content/abstract)
  ()
  (:documentation "A BORDER COMPONENT."))
