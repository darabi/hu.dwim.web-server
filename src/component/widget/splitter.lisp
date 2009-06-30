;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Splitter widget

(def (component e) splitter/widget (widget/basic list/layout)
  ())

(def (macro e) splitter/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'splitter ,@args :contents (list ,@contents)))

(def render-xhtml splitter/widget
  (not-yet-implemented))
