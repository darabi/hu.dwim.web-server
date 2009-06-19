;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Closable mixin

(def (component e) closable/mixin ()
  ()
  (:documentation "A COMPONENT that is permanently closable."))

(def layered-method make-move-commands ((component closable/mixin) class prototype value)
  (optional-list* (make-close-component-command component class prototype value) (call-next-method)))

(def layered-method make-close-component-command ((component closable/mixin) class prototype value)
  (command ()
    (icon close)
    (make-component-action component
      (close-component component class prototype value))))

(def layered-method close-component ((component closable/mixin) class prototype value)
  (remove-place (make-component-place component)))
