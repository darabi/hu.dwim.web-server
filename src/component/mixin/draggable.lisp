;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Draggable mixin

(def (component e) draggable/mixin ()
  ()
  (:documentation "A COMPONENT that can be dragged on the remote side."))

(def render-component draggable/mixin
  (not-yet-implemented))

;;;;;;
;;; Drop place mixin

(def (component e) drop-place/mixin ()
  ()
  (:documentation "A COMPONENT that serves as a drag and drop place."))

(def render-component drop-place/mixin
  (not-yet-implemented))
