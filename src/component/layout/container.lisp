;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Container abstract

(def (component ea) container/abstract (contents/mixin)
  ()
  (:documentation "A container component with several components inside."))

;;;;;;
;;; Container basic

(def (component ea) container/basic (container/abstract component/basic)
  ())

(def (macro e) container/basic ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'container/basic :contents (list ,@contents)))

(def render container/basic
  <div ,(call-next-method)>)

;;;;;;
;;; container full

(def (component ea) container/full (container/basic component/full)
  ())

(def (macro e) container/full ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'container/full ,@args :contents (list ,@contents)))

(def render container/full
  (with-render-style/abstract (-self-)
    (call-next-method)))

;;;;;;
;;; Container

(def (macro e) container ((&rest args &key &allow-other-keys) &body contents)
  (if args
      (container/full args ,@contents)
      (container/basic args ,@contents)))
