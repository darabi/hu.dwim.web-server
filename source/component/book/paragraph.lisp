;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; paragraph/viewer

(def (component e) paragraph/viewer (viewer/basic contents/abstract style/mixin)
  ((style-class "paragraph")))

(def macro paragraph/viewer ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'paragraph/viewer ,@args :contents (list ,@contents)))

(def render-xhtml paragraph/viewer
  (with-render-style/mixin (-self- :element-name "p")
    (call-next-method)))

(def layered-function render-paragraph (component)
  (:method :in xhtml-layer ((self number))
    <p ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <p ,(render-component self)>)

  (:method ((self component))
    (render-component self)))
