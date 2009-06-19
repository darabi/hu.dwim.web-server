;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Button abstract

(def (component e) button/abstract ()
  ())

;;;;;;
;;; Push button basic

(def (component e) push-button/basic (button/abstract)
  ())

;;;;;;
;;; Toggle button basic

(def (component e) toggle-button/basic (button/abstract)
  ())
