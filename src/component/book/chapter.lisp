;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Chapter basic

(def component chapter/basic (content/basic title/mixin)
  ())

(def render-xhtml chapter/basic
  <div ,(render-title -self-)
       ,(call-next-method)>)
