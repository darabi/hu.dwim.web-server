;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Button widget

(def (component e) button/widget (widget/basic)
  ())

;;;;;;
;;; Push button widget

(def (component e) push-button/widget (button/widget)
  ())

;;;;;;
;;; Toggle button widget

(def (component e) toggle-button/widget (button/widget)
  ())
