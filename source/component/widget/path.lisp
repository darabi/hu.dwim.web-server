;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; path/widget

(def (component e) path/widget (standard/widget contents/component)
  ())

(def (macro e) path/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'path/widget ,@args :contents (list ,@contents)))

(def refresh-component path/widget
  (bind (((:slots contents) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-))
         (component-value (component-value-of -self-)))
    (if contents
        (foreach [setf (component-value-of !1) !2] contents component-value)
        (setf contents (mapcar [make-content-presentation -self- dispatch-class dispatch-prototype !1] component-value)))))

(def render-xhtml path/widget
  <span (:class "path")
        ,(iter (for content :in (contents-of -self-))
               (unless (first-iteration-p)
                 `xml," / ")
               (render-component content))>)
