;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Cells mixin

(def (component e) cells/mixin ()
  ((cells :type components)))

(def (layered-function e) render-cells (component)
  (:method ((self cells/mixin))
    (foreach #'render-component (cells-of self))))

;;;;;;
;;; Cell basic

(def (component e) cell/basic (style/abstract content/mixin)
  ((column-span nil :type integer)
   (row-span nil :type integer)
   (word-wrap :type boolean)
   (horizontal-alignment nil :type (member nil :left :center :right))
   (vertical-alignment nil :type (member nil :top :center :bottom))))

(def with-macro* render-cell/basic (cell &key css-class)
  (setf css-class (ensure-list css-class))
  (if (typep cell 'cell/basic)
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

(def render-xhtml cell/basic
  (render-cell/basic (-self-)
    (call-next-method)))

(def render-ods cell/basic
  <table:table-cell ,(call-next-method)>)

(def (layered-function e) render-cells (table row column cell)
  (:method :before ((table table/mixin) (row row/basic) (column column-component) (cell cell/basic))
    (ensure-refreshed cell))

  (:method ((table table/mixin) (row row/basic) (column column-component) (cell cell/basic))
    (render-component cell))

  (:method :in xhtml-layer ((table table/mixin) (row row/basic) (column column-component) (cell component))
    <td ,(render-component cell)>)

  (:method :in xhtml-layer ((table table/mixin) (row row/basic) (column column-component) (cell string))
    <td ,(render-component cell)>)

  (:method :in ods-layer ((table table/mixin) (row row/basic) (column column-component) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table table/mixin) (row row/basic) (column column-component) (cell string))
    <table:table-cell ,(render-component cell)>))
