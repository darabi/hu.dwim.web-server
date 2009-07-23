;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; chapter/viewer

(def (component e) chapter/viewer (viewer/basic contents/abstract title/mixin)
  ())

(def (macro e) chapter/viewer ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter/viewer ,@args :contents (list ,@contents)))

(def render-xhtml chapter/viewer
  <div (:class "chapter")
    ,(render-title-for -self-)
    ,(render-contents-for -self-)>)
