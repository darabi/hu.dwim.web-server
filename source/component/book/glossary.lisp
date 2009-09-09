;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; glossary/inspector

(def (component e) glossary/inspector (inspector/basic)
  ())

(def (macro e) glossary/inspector (&rest args &key &allow-other-keys)
  `(make-instance 'glossary/inspector ,@args))

(def render-xhtml glossary/inspector
  (not-yet-implemented))
