;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; glossary/viewer

(def (component e) glossary/viewer (viewer/basic)
  ())

(def (macro e) glossary/viewer (&rest args &key &allow-other-keys)
  `(make-instance 'glossary/viewer ,@args))

(def render-xhtml glossary/viewer
  (not-yet-implemented))
