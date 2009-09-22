;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; symbol/special-variable-name/inspector

(def (component e) symbol/special-variable-name/inspector (t/inspector)
  ())

(def (macro e) symbol/special-variable-name/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'symbol/special-variable-name/inspector ,@args :component-value ,(the-only-element name)))