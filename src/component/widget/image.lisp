;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; image/widget

(def (component e) image/widget (widget/basic)
  ((path :type string))
  (:documentation "An IMAGE specified by a file system path."))

(def (macro e) image/widget (&rest args &key path &allow-other-keys)
  (declare (ignore path))
  `(make-instance 'image/widget ,@args))

(def render-component image/widget
  (render-image -self-))

(def (function e) render-image (self)
  <img (:id ,(id-of self) :class ,(style-class-of self) :style ,(custom-style-of self) :src ,(path-of self))>)
