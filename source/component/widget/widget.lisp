;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; widget/abstract

(def (component e) widget/abstract ()
  ()
  (:documentation "A WIDGET/ABSTRACT has visual appearance on its own, it also provides behaviour on the client side such as visiblility, expanding, scrolling, resizing, context menu, selection, sorting subparts, etc."))

;;;;;;
;;; widget/minimal

(def (component e) widget/minimal (widget/abstract component/minimal)
  ())

;;;;;;
;;; widget/basic

(def (component e) widget/basic (widget/minimal component/basic)
  ())

;;;;;;
;;; widget/style

(def (component e) widget/style (widget/basic component/style)
  ())

;;;;;;
;;; widget/full

(def (component e) widget/full (widget/style component/full)
  ())
