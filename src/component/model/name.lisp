;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; symbol/definition-name/inspector

(def (component e) symbol/definition-name/inspector (t/inspector)
  ())

(def (macro e) symbol/definition-name/inspector ((&rest args &key &allow-other-keys) &body name)
  `(make-instance 'symbol/definition-name/inspector ,@args :component-value ,(the-only-element name)))
