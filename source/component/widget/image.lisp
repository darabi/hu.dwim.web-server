;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; image/widget

(def (component e) image/widget (standard/widget)
  ((location :type uri))
  (:documentation "An IMAGE specified by an URI location"))

(def (macro e) image/widget (&rest args &key location &allow-other-keys)
  (declare (ignore location))
  `(make-instance 'image/widget ,@args))

(def render-component image/widget
  (render-image -self-))

(def (function e) render-image (self)
  (bind (((:read-only-slots id style-class custom-style location) self))
    <img (:id ,id :class ,style-class :style ,custom-style :src ,(print-uri-to-string location) :alt "")>))
