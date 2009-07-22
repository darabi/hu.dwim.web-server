;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; pathname/inspector

(def (component e) pathname/inspector (t/inspector)
  ())

;;;;;;
;;; pathname/file/inspector

(def (component e) pathname/file/inspector (t/inspector)
  ())

(def (macro e) pathname/file/inspector ((&rest args &key &allow-other-keys) &body path)
  `(make-instance 'pathname/file/inspector ,@args :component-value ,(the-only-element path)))

;;;;;;
;;; lisp-source-file/inspector

(def (component e) pathname/lisp-source-file/inspector (t/inspector)
  ())
