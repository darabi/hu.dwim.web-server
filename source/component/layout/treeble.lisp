;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; treeble/layout

(def (component e) treeble/layout (standard/layout tree/component root-nodes/mixin)
  ())

(def (macro e) treeble/layout ((&rest args &key &allow-other-keys) &body nodes)
  `(make-instance 'treeble/layout ,@args :root-nodes (list ,@nodes)))

(def render-xhtml treeble/layout
  (bind (((:read-only-slots root-nodes) -self-))
    <table (:class "treeble layout")
      <tbody ,(foreach #'render-component root-nodes)>>))
