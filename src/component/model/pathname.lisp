;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; pathname/inspector

(def (component e) pathname/inspector (t/inspector)
  ())

(def (macro e) pathname/inspector (pathname &rest args)
  `(make-instance 'pathname/inspector :component-value ,pathname ,@args))

;;;;;;
;;; pathname/text-file/inspector

(def (component e) pathname/text-file/inspector (inspector/basic content/widget)
  ())

(def (macro e) pathname/text-file/inspector (pathname &rest args)
  `(make-instance 'pathname/text-file/inspector :component-value ,pathname ,@args))

(def refresh-component pathname/text-file/inspector
  (bind (((:slots component-value content) -self-))
    (setf content (read-file-into-string component-value))))

(def render-xhtml pathname/text-file/inspector
  (with-render-style/mixin (-self- :element-name "pre")
    (render-content-for -self-)))
