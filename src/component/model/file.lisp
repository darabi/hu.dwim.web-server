;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; source-file/inspector

(def (component e) source-file/inspector (t/inspector)
  ())

(def (macro e) source-file/inspector ((&rest args &key &allow-other-keys) &body file)
  `(make-instance 'source-file/inspector ,@args :component-value ,(the-only-element file)))
