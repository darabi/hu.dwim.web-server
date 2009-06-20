;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Word index basic 

(def (component e) word-index/basic (component/basic)
  ())

(def (macro e) word-index ()
  `(make-instance 'word-index/basic))

(def render-xhtml word-index/basic
  (not-yet-implemented))
