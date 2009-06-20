;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Chapter basic

(def (component e) chapter/basic (component/basic contents/abstract title/mixin)
  ())

(def (macro e) chapter/basic ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter/basic ,@args :contents (list ,@contents)))

(def render-xhtml chapter/basic
  (bind (((:read-only-slots title contents) -self-))
    <div (:class "chapter")
         ,(render-title title)
         ,(foreach #'render-paragraph contents)>))

;;;;;;
;;; Chapter

(def (macro e) chapter ((&rest args &key &allow-other-keys) &body contents)
  `(chapter/basic ,args ,@contents))
