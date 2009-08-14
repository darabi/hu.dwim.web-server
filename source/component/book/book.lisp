;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; book/viewer

(def (component e) book/viewer (viewer/basic)
  ())

(def (macro e) book/viewer ((&rest args &key &allow-other-keys) &body book)
  `(make-instance 'book/viewer ,@args :component-value ,(the-only-element book)))

(def render-xhtml book/viewer
  <div (:class "book")>)

;;;;;;
;;; Author

(def (layered-function e) render-author (component)
  (:method :in xhtml-layer ((self number))
    <div (:class "author")
      ,(render-component self)>)

  (:method :in xhtml-layer ((self string))
    <div (:class "author")
      ,(render-component self)>)

  (:method ((self component))
    (render-component self)))
