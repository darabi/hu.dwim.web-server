;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; File inspector

(def (component e) file/inspector ()
  ())

(def render-xhtml file/inspector
  (not-yet-implemented))

;;;;;;
;;; Source file inspector

(def (component e) source-file/inspector (file/inspector)
  ())

(def render-xhtml source-file/inspector
  (not-yet-implemented))
