;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Thunk mixin

(def (component e) thunk/mixin ()
  ((thunk :type function))
  (:documentation "A COMPONENT with a function to be called in RENDER-COMPONENT."))

(def render-component thunk/mixin
  (funcall (the function (thunk-of -self-))))

;;;;;;
;;; Inline basic

(def (component e) inline/basic (thunk/mixin)
  ())

(def (macro e) inline/basic (&body forms)
  `(make-instance 'inline/basic :thunk (lambda () ,@forms)))

(def (macro e) inline (&body forms)
  `(inline/basic ,@forms))

;;;;;;
;;; Wrap render basic

(def (component e) wrap-render/basic (thunk/mixin content/mixin)
  ())

(def render-component wrap-render/basic
  (funcall (thunk-of -self-) #'call-next-method))

(def (macro e) wrap-render/basic (content &body forms)
  `(make-instance 'wrap-render/basic
                  :thunk (lambda (next-method)
                           (flet ((-body- ()
                                    (funcall next-method)))
                             ,@forms))
                  :content ,content))

(def (macro e) wrap-render (content &body forms)
  `(wrap-render/basic ,content ,@forms))
