;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Remote setup mixin

(def (component e) remote-setup/mixin (frame-unique-id/mixin)
  ()
  (:documentation "A COMPONENT that will be unconditionally set up on the remote side."))

(def (layered-function e) render-remote-setup (component)
  (:method :in xhtml-layer ((self id/mixin))
    `js(on-load (wui.setup-component ,(id-of self) ,(instance-class-name-as-string self)))))

(def render-xhtml :after remote-setup/mixin
  (render-remote-setup -self-))