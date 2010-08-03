;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Constants

(def (constant e) +table-cell-horizontal-alignment-style-class/left+ "_hla")
(def (constant e) +table-cell-horizontal-alignment-style-class/center+ "_hca")
(def (constant e) +table-cell-horizontal-alignment-style-class/right+ "_hra")

(def (constant e) +table-cell-vertical-alignment-style-class/top+ "_vta")
(def (constant e) +table-cell-vertical-alignment-style-class/center+ "_vca")
(def (constant e) +table-cell-vertical-alignment-style-class/bottom+ "_vba")

(def (constant e) +table-cell-nowrap-style-class+ "_nw")

;;;;;;
;;; cell/widget

(def (component e) cell/widget (standard/widget
                                content/component
                                context-menu/mixin
                                selectable/mixin)
  ((column-span nil :type integer)
   (row-span nil :type integer)
   (word-wrap :type boolean)
   (horizontal-alignment nil :type (member nil :left :center :right))
   (vertical-alignment nil :type (member nil :top :center :bottom))))

(def (macro e) cell/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'cell/widget ,@args :content ,(the-only-element content)))

(def render-xhtml cell/widget
  (bind (((:read-only-slots column-span row-span horizontal-alignment vertical-alignment id) -self-)
         (horizontal-style-class (ecase horizontal-alignment
                                   (:right +table-cell-horizontal-alignment-style-class/right+)
                                   (:center +table-cell-horizontal-alignment-style-class/center+)
                                   ((:left nil) nil)))
         (vertical-style-class (ecase vertical-alignment
                                 (:top +table-cell-vertical-alignment-style-class/top+)
                                 (:bottom +table-cell-vertical-alignment-style-class/bottom+)
                                 ((:center nil) nil)))
         (word-wrap-style-class (when (slot-boundp -self- 'word-wrap)
                                  (ecase (slot-value -self- 'word-wrap)
                                    ((#f) +table-cell-nowrap-style-class+)
                                    ((#t) nil)))))
    <td (:id ,id
         :class `str("cell widget " ,horizontal-style-class " " ,vertical-style-class " " ,word-wrap-style-class)
         :colspan ,column-span
         :rowspan ,row-span)
      ,(render-content-for -self-)>))

(def render-ods cell/widget
  <table:table-cell
    ,(render-content-for -self-)>)

(def (layered-function e) render-table-row-cell (table row column cell)
  (:method :before ((table table/widget) (row row/widget) (column column/widget) (cell cell/widget))
    (ensure-refreshed cell))

  (:method ((table table/widget) (row row/widget) (column column/widget) (cell cell/widget))
    (render-component cell))

  (:method :in xhtml-layer ((table table/widget) (row row/widget) (column column/widget) (cell component))
    <td (:class "cell widget") ,(render-component cell)>)

  (:method :in xhtml-layer ((table table/widget) (row row/widget) (column column/widget) (cell string))
    <td (:class "cell widget") ,(render-component cell)>)

  (:method :in ods-layer ((table table/widget) (row row/widget) (column column/widget) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table table/widget) (row row/widget) (column column/widget) (cell string))
    <table:table-cell ,(render-component cell)>))
