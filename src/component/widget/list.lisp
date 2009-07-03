;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; List widget

(def (component e) list/widget (widget/basic list/layout)
  ()
  (:documentation "A LIST/WIDGET with several COMPONENTs inside."))

(def (macro e) list/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'list/widget ,@args :contents (list ,@contents)))
