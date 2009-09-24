;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; symbol/type-name/inspector

(def (component e) symbol/type-name/inspector (t/inspector)
  ())

(def (macro e) symbol/type-name/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'symbol/type-name/inspector ,@args :component-value ,(the-only-element name)))
