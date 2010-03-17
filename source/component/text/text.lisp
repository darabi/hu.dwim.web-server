;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; text/inspector

(def (component e) text/inspector (t/inspector)
  ())

;;;;;;
;;; t/text/inspector

(def (component e) t/text/inspector (inspector/style t/detail/inspector contents/widget)
  ())

(def refresh-component t/text/inspector
  (bind (((:slots contents component-value) -self-))
    (setf contents (mapcar [make-value-inspector !1
                                                 :initial-alternative-type 't/text/inspector
                                                 :edited (edited-component? -self-)
                                                 :editable (editable-component? -self-)]
                           (contents-of component-value)))))

(def method render-command-bar-for-alternative? ((component t/text/inspector))
  #f)
