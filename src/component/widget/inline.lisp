;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Thunk mixin

(def (component e) thunk/mixin ()
  ((thunk :type (or symbol function)))
  (:documentation "A COMPONENT with a FUNCTION."))

;;;;;;
;;; Inline basic

(def (component e) inline/basic (thunk/mixin)
  ()
  (:documentation "A COMPONENT with a FUNCTION to be called in RENDER-COMPONENT."))

(def render-component inline/basic
  (funcall (thunk-of -self-)))

(def (macro e) inline/basic (&body forms)
  `(make-instance 'inline/basic :thunk (lambda () ,@forms)))

;;;;;;
;;; Wrapper basic

(def (component e) wrapper/basic (content/mixin thunk/mixin)
  ()
  (:documentation "A COMPONENT that has another COMPONENT inside wrapped with a rendering function."))

(def render-component wrapper/basic
  (funcall (thunk-of -self-) #'call-next-method))

(def (macro e) wrapper/basic (content &body forms)
  `(make-instance 'wrapper/basic
                  :thunk (lambda (next-method)
                           (flet ((-body- ()
                                    (funcall next-method)))
                             ,@forms))
                  :content ,content))

(def (macro e) wrapper (content &body forms)
  `(wrapper/basic ,content ,@forms))
