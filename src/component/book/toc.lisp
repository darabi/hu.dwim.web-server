;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; TOC viewer

(def (component e) toc/viewer ()
  ())

(def (macro e) toc/viewer ()
  `(make-instance 'toc/viewer))

(def render-xhtml toc/viewer
  (not-yet-implemented))
