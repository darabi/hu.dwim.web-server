;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Row layout

(def (component e) row/layout (layout/minimal row/abstract cells/mixin)
  ((vertical-alignment nil :type (member nil :top :center :bottom))))

(def (macro e) row/layout ((&rest args &key &allow-other-keys) &body cells)
  `(make-instance 'row/layout ,@args :cells (list ,@cells)))

(def render-xhtml row/layout
  <tr (:class "row layout")
    ,(render-cells-for -self-)>)
