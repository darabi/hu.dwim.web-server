;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Glossary basic

(def (component e) glossary/basic (component/basic)
  ())

(def (macro e) glossary (&rest args &key &allow-other-keys)
  `(make-instance 'glossary/basic ,@args))

(def render-xhtml glossary/basic
  (not-yet-implemented))
