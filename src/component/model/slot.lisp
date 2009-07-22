;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; standard-slot-definition/inspector

(def (component e) standard-slot-definition/inspector (t/inspector)
  ())

(def (macro e) standard-slot-definition/inspector ((&rest args &key &allow-other-keys) &body slot)
  `(make-instance 'standard-slot-definition/inspector ,@args :component-value ,(the-only-element slot)))
