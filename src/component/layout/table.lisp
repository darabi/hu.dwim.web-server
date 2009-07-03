;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table layout

(def (component e) table/layout (layout/minimal rows/mixin)
  ()
  (:documentation "A LAYOUT that positions a SEQUENCE of ROWS in a TABLE. The CELLs are positioned in the corresponding COLUMNs based on their indices."))

(def (macro e) table/layout ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'table/layout ,@args :rows (list ,@rows)))

(def render-xhtml table/layout
  <table
    <tbody ,(render-rows -self-)>>)
