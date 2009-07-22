;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-class/inspector

(def (component e) standard-class/inspector (t/inspector)
  ())

(def (macro e) standard-class/inspector ((&rest args &key &allow-other-keys) &body class)
  `(make-instance 'standard-class/inspector ,@args :component-value ,(the-only-element class)))
