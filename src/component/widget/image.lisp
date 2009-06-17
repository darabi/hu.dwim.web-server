;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Image abstract

(def (component ea) image/abstract ()
  ((path :type string)))

(def (macro e) image ((&rest args &key &allow-other-keys) &body path)
  `(make-instance 'image/basic ,@args :path ,(the-only-one path)))

;;;;;;
;;; Image basic

(def (component ea) image/basic (image/abstract)
  ())

(def render image/basic
  (render-image -self-))

(def (layered-function e) render-image (component)
  (:method ((self image/basic))
    <img (:src ,(path-of -self-))>))

;;;;;;
;;; Image full

(def (component ea) image/full (image/basic style/abstract tooltip/mixin)
  ())

(def layered-method render-image ((self image/full))
  <img (:id ,(id-of self) :class ,(style-class-of self) :style ,(custom-style-of self) :src ,(path-of self))>)

;;;;;;
;;; Image mixin

(def (component ea) image/mixin ()
  ((image :type component))
  (:documentation "A component with an image."))

(def layered-method render-image ((self image/mixin))
  (render-component (image-of self)))
