;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Constants

(def (constant e :test 'string=) +table-cell-horizontal-alignment-css-class/left+ "_hla")
(def (constant e :test 'string=) +table-cell-horizontal-alignment-css-class/center+ "_hca")
(def (constant e :test 'string=) +table-cell-horizontal-alignment-css-class/right+ "_hra")

(def (constant e :test 'string=) +table-cell-vertical-alignment-css-class/top+ "_vta")
(def (constant e :test 'string=) +table-cell-vertical-alignment-css-class/center+ "_vca")
(def (constant e :test 'string=) +table-cell-vertical-alignment-css-class/bottom+ "_vba")

(def (constant e :test 'string=) +table-cell-nowrap-css-class+ "_nw")

;;;;;;
;;; cell/widget

(def (component e) cell/widget (widget/style
                                content/abstract
                                context-menu/mixin
                                selectable/mixin)
  ((column-span nil :type integer)
   (row-span nil :type integer)
   (word-wrap :type boolean)
   (horizontal-alignment nil :type (member nil :left :center :right))
   (vertical-alignment nil :type (member nil :top :center :bottom))))

(def (macro e) cell/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'cell/widget ,@args :content ,(the-only-element content)))

(def with-macro* render-cell/widget (cell &key css-class)
  (setf css-class (ensure-list css-class))
  (if (typep cell 'cell/widget)
      (bind (((:read-only-slots column-span row-span horizontal-alignment vertical-alignment) cell))
        (ecase horizontal-alignment
          (:right (push +table-cell-horizontal-alignment-css-class/right+ css-class))
          (:center (push +table-cell-horizontal-alignment-css-class/center+ css-class))
          ((:left nil) nil))
        (ecase vertical-alignment
          (:top (push +table-cell-vertical-alignment-css-class/top+ css-class))
          (:bottom (push +table-cell-vertical-alignment-css-class/bottom+ css-class))
          ((:center nil) nil))
        (when (slot-boundp cell 'word-wrap)
          (ecase (slot-value cell 'word-wrap)
            ((#f) (push +table-cell-nowrap-css-class+ css-class))
            ((#t) nil)))
        <td (:class ,(join-strings css-class)
             :colspan ,column-span
             :rowspan ,row-span)
            ,(-body-)>)
      <td (:class ,(join-strings css-class))
        ,(-body-)>))

(def render-xhtml cell/widget
  (render-cell/widget (-self-)
    (render-content-for -self-)))

(def render-ods cell/widget
  <table:table-cell
    ,(render-content-for -self-)>)

(def (layered-function e) render-table-row-cell (table row column cell)
  (:method :before ((table table/widget) (row row/widget) (column column/widget) (cell cell/widget))
    (ensure-refreshed cell))

  (:method ((table table/widget) (row row/widget) (column column/widget) (cell cell/widget))
    (render-component cell))

  (:method :in xhtml-layer ((table table/widget) (row row/widget) (column column/widget) (cell component))
    <td ,(render-component cell)>)

  (:method :in xhtml-layer ((table table/widget) (row row/widget) (column column/widget) (cell string))
    <td ,(render-component cell)>)

  (:method :in ods-layer ((table table/widget) (row row/widget) (column column/widget) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table table/widget) (row row/widget) (column column/widget) (cell string))
    <table:table-cell ,(render-component cell)>))
