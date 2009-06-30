;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Draggable abstract

(def (component e) draggable/abstract ()
  ()
  (:documentation "A COMPONENT that can be dragged on the remote side."))

;;;;;;
;;; Drag and drop place abstract

(def (component e) drag-and-drop-place/abstract ()
  ()
  (:documentation "A COMPONENT that serves as a DRAG-AND-DROP-PLACE."))
