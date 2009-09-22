;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; chapter/inspector

(def (component e) chapter/inspector (t/inspector exportable/abstract)
  ())

(def (macro e) chapter/inspector ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'chapter/inspector ,@args :contents (list ,@contents)))

(def layered-method find-inspector-type-for-prototype ((prototype chapter))
  'chapter/inspector)

(def layered-method make-alternatives ((component chapter/inspector) class prototype (value chapter))
  (list* (delay-alternative-component-with-initargs 'chapter/text/inspector :component-value value)
         (call-next-method)))

;;;;;;
;;; chapter/text/inspector

(def (component e) chapter/text/inspector (t/text/inspector collapsible/abstract title/mixin)
  ())


(def render-xhtml chapter/text/inspector
  (with-render-style/abstract (-self-)
    (render-collapse-or-expand-command-for -self-)
    (render-title-for -self-)
    <br>
    (when (expanded-component? -self-)
      (render-contents-for -self-))))

(def render-text chapter/text/inspector
  (write-text-line-begin)
  (render-title-for -self-)
  (write-text-line-separator)
  (call-next-method))

(def layered-method make-title ((self chapter/text/inspector) class prototype (value chapter))
  (title-of value))
