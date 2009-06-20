;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Book basic

(def (component e) book/basic (title/mixin)
  ((author nil :type (or null component))
   (toc nil :type (or null component))
   (chapters :type components)
   (index nil :type (or null component))
   (glossary nil :type (or null component))))

(def (macro e) book/basic ((&rest args &key &allow-other-keys) &body chapters)
  `(make-instance 'book/basic ,@args :chapters (list ,@chapters)))

(def render-xhtml book/basic
  (bind (((:read-only-slots title author toc chapters index glossary) -self-))
    <div (:class "book")
      ,(render-title title)
      ,(when author
         (render-author author))
      ,(when toc
         (render-component toc))
      ,(foreach #'render-component chapters)
      ,(when index
         (render-component index))
      ,(when glossary
         (render-component glossary))>))

;;;;;;
;;; Book

(def (macro e) book ((&rest args &key &allow-other-keys) &body chapters)
  `(book/basic ,args ,@chapters))

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
