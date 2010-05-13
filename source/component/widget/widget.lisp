;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/widget

(def (component e) component/widget ()
  ()
  (:documentation "A widget is similar a layout but it also provides behaviour on the client side. A few examples are visiblility, expanding, collapsing, scrolling, resizing, moving, context menu, selection, sorting subparts, etc. This class does not have any slots on purpose."))

;;;;;;
;;; standard/widget

(def (component e) standard/widget (component/widget standard/component)
  ())
