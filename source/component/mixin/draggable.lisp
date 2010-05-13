;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; draggable/component

(def (component e) draggable/component ()
  ()
  (:documentation "A COMPONENT that can be dragged on the remote side."))

;;;;;;
;;; drag-and-drop-place/component

(def (component e) drag-and-drop-place/component ()
  ()
  (:documentation "A COMPONENT that serves as a DRAG-AND-DROP-PLACE."))
