;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; component/layout

(def (component e) component/layout ()
  ()
  (:documentation "A layout is a visual arrangement of its child components. It does not provide behavior on the client side to modify the component's state or appearance. On the other hand it may have various properties that control the style and look and feel. This class does not have any slots on purpose."))

;;;;;;
;;; standard/layout

(def (component e) standard/layout (component/layout standard/component)
  ()
  (:documentation "A standard layout includes a set of generally useful mixins."))
