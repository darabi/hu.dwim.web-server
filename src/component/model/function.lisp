;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; function/inspector

(def (component e) function/inspector (t/inspector)
  ())

(def (macro e) function/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'function/inspector ,@args :component-value ,(the-only-element name)))
