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
;;; Inline widget

(def (component e) inline/widget (widget/basic thunk/mixin)
  ()
  (:documentation "A COMPONENT with a FUNCTION that is called in RENDER-COMPONENT."))

(def (macro e) inline/widget (&body forms)
  `(make-instance 'inline/widget :thunk (lambda () ,@forms)))

(def render-component inline/widget
  (funcall (thunk-of -self-)))

;;;;;;
;;; Wrapper widget

(def (component e) wrapper/widget (widget/basic content/abstract thunk/mixin)
  ()
  (:documentation "A COMPONENT that has another COMPONENT inside wrapped with a rendering function. The content can be rendered by calling the local function (-body-)."))

(def (macro e) wrapper/widget (content &body forms)
  `(make-instance 'wrapper/widget
                  :thunk (lambda (next-method)
                           (flet ((-body- ()
                                    (funcall next-method)))
                             ,@forms))
                  :content ,content))

(def render-component wrapper/widget
  (funcall (thunk-of -self-) (lambda () (render-content-for -self-))))
