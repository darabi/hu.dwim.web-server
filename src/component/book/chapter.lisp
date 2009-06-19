;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Chapter basic

(def (component e) chapter/basic (content/basic title/mixin)
  ())

(def (macro e) chapter/basic ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter/basic ,@args :contents (list ,@contents)))

(def render-xhtml chapter/basic
  <div (:class "chapter")
    ,(render-title -self-)
    ,(call-next-method)>)
