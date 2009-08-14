;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; border/widget

(def (component e) border/widget (widget/basic content/abstract)
  ()
  (:documentation "A BORDER COMPONENT."))
