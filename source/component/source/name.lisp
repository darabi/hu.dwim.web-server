;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; symbol/definition-name/inspector

(def (component e) symbol/definition-name/inspector (inspector/basic contents/widget)
  ())

(def (macro e) symbol/definition-name/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'symbol/definition-name/inspector ,@args :component-value ,(the-only-element name)))

(def refresh-component symbol/definition-name/inspector
  (bind (((:slots contents component-value) -self-)
         (dispatch-class (component-dispatch-class -self-))
         (dispatch-prototype (component-dispatch-prototype -self-)))
    (setf contents (make-definition-name/contents -self- dispatch-class dispatch-prototype component-value))))

(def generic make-definition-name/contents (component class prototype value)
  (:method ((component symbol/definition-name/inspector) class prototype value)
    (iter (for specification :in (swank-backend:find-definitions value))
          (awhen (case (caar specification)
                   (defclass (make-value-inspector (find-class value)))
                   (defun (make-value-inspector (symbol-function value)))
                   (defmacro (make-value-inspector (macro-function value)))
                   (defgeneric (make-value-inspector (symbol-function value)))
                   ;; TODO:
                   (defvar nil)
                   (t nil))
            (collect it)))))
