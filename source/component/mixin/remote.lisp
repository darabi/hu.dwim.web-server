;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; remote-setup/mixin

(def (component e) remote-setup/mixin (frame-unique-id/mixin)
  ((remote-setup
    #f
    :type boolean))
  (:documentation "A COMPONENT that will be conditionally set up on the remote side."))

(def (layered-function e) render-remote-setup (component)
  (:method :around ((self id/mixin))
    (when (remote-setup? self)
      (call-next-layered-method)))

  (:method :in xhtml-layer ((self id/mixin))
    `js-onload(wui.setup-component ,(id-of self) ,(instance-class-name-as-string self))))

(def render-xhtml :after remote-setup/mixin
  (render-remote-setup -self-))
