;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Closable abstract

(def (component e) closable/abstract ()
  ()
  (:documentation "A COMPONENT that is permanently closable."))

(def layered-method make-move-commands ((component closable/abstract) class prototype value)
  (optional-list* (make-close-component-command component class prototype value) (call-next-method)))

(def layered-method close-component ((component closable/abstract) class prototype value)
  (remove-place (make-component-place component)))
