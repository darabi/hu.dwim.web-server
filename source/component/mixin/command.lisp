;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; commands/mixin

(def (component e) commands/mixin (menu-bar/mixin context-menu/mixin command-bar/mixin)
  ()
  (:documentation "A COMPONENT with various COMPONENTs providing behaviour through COMMANDs."))

(def (generic e) command-position (component)
  (:method ((self number))
    most-positive-fixnum)

  (:method ((self string))
    most-positive-fixnum)

  (:method ((self component))
    most-positive-fixnum))
