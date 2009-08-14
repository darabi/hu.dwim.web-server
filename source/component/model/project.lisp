;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; project/inspector

(def (component e) project/inspector (t/inspector)
  ())

(def (macro e) project/inspector (project &rest args &key &allow-other-keys)
  `(make-instance 'project/inspector ,project ,@args))

(def layered-method find-inspector-type-for-prototype ((prototype project))
  'project/inspector)

(def layered-method make-alternatives ((component project/inspector) class prototype value)
  (list* (delay-alternative-component-with-initargs 'project/detail/inspector :component-value value)
         (call-next-method)))

(def method localized-instance-name ((project project))
  (name-of project))

;;;;;;
;;; project/detail/inspector

(def (component e) project/detail/inspector (inspector/basic widget/style)
  ((directory :type component)))

(def refresh-component project/detail/inspector
  (bind (((:slots directory) -self-)
         (component-value (component-value-of -self-)))
    (setf directory (make-value-inspector (path-of component-value) :initial-alternative-type 'sequence/list/inspector))))

(def render-xhtml project/detail/inspector
  (bind (((:read-only-slots directory component-value) -self-))
    (with-render-style/abstract (-self-)
      <div "Project name" ,(name-of component-value)>
      <div "Project contents">
      (render-component directory))))
