;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Lisp form

(def (component e) lisp-form ()
  ((form :type t)))

(def render-component lisp-form
  <pre (:class "lisp-form")
       ,(princ (form-of -self-))>)