;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; splitter/widget

(def (component e) splitter/widget (widget/basic list/layout)
  ())

(def (macro e) splitter/widget ((&rest args &key &allow-other-keys) &body contents)
  `(make-instance 'splitter ,@args :contents (list ,@contents)))

(def render-xhtml splitter/widget
  (not-yet-implemented))
