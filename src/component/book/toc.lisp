;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table of contents basic

(def (component e) table-of-contents/basic ()
  ())

(def (macro e) table-of-contents ()
  `(make-instance 'word-index/basic))

(def render-xhtml table-of-contents/basic
  (not-yet-implemented))
