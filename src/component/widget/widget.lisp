;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Widget abstract

(def (component e) widget/abstract ()
  ()
  (:documentation "A WIDGET/ABSTRACT has visual appearance on its own, it also provides behaviour on the client side such as visiblility, expanding, scrolling, resizing, context menu, selection, sorting subparts, etc."))

;;;;;;
;;; Widget minimal

(def (component e) widget/minimal (widget/abstract component/minimal)
  ())

;;;;;;
;;; Widget basic

(def (component e) widget/basic (widget/minimal component/basic)
  ())

;;;;;;
;;; Widget style

(def (component e) widget/style (widget/basic component/style)
  ())

;;;;;;
;;; Widget full

(def (component e) widget/full (widget/style component/full)
  ())