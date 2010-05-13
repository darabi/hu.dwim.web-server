;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; symbol/definition-name/inspector

(def (component e) symbol/definition-name/inspector (t/inspector contents/widget)
  ())

(def refresh-component symbol/definition-name/inspector
  (bind (((:slots contents component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-)))
    (setf contents (make-definition-name/contents -self- dispatch-class dispatch-prototype component-value))))

(def generic make-definition-name/contents (component class prototype value)
  (:method ((component symbol/definition-name/inspector) class prototype value)
    (mapcar #'make-value-inspector (make-definitions value))))
