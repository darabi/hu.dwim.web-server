;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Content

(def component content-component ()
  ((content nil :type component)))

(def render content-component ()
  (render (content-of -self-)))

(def method find-command-bar ((component content-component))
  (find-command-bar (content-of component)))

;;;;;;
;;; Top

(def component top-component (content-component)
  ()
  (:documentation "The top command will replace the content of a top-component with the component which the action refers to."))

;;;;;;
;;; Empty

(def component empty-component ()
  ())

(def render empty-component ()
  +void+)

;;;;;;;
;;; Label

(def component label-component ()
  ((component-value)))

(def render label-component ()
  <span ,(component-value-of -self-)>)

;;;;;;
;;; Delay

(def component inline-component ()
  ((thunk)))

(def render inline-component ()
  (funcall (thunk-of -self-)))

(def (macro e) make-inline-component (&body forms)
  `(make-instance 'inline-component :thunk (lambda () ,@forms)))

;;;;;;
;;; Wrapper

(def component wrapper-component ()
  ((thunk)
   (body)))

(def render wrapper-component ()
  (with-slots (thunk body) -self-
    (funcall thunk (lambda () (render body)))))

(def (macro e) make-wrapper-component (&rest args &key wrap &allow-other-keys)
  (remove-from-plistf args :wrap)
  `(make-instance 'wrapper-component
                  :thunk (lambda (body-thunk)
                           (flet ((body ()
                                    (funcall body-thunk)))
                             ,wrap))
                  ,@args))

;;;;;;
;;; Detail

(def component detail-component ()
  ())

;;;;;;
;;; Frame

(def component frame-component (top-component)
  ((stylesheets nil)
   (title nil)))

(def render frame-component ()
  (with-slots (stylesheets title content) -self-
    <html
     <head
      <meta (:http-equiv "Content-Type" :content "text/html; charset=utf-8")>
      <title ,title>
      ,@(mapcar (lambda (stylesheet)
                  <link (:rel "stylesheet" :type "text/css" :href ,stylesheet)>)
                stylesheets)>
     <body ,(render content)>>))
