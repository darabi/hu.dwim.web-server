;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

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
;;; Table abstract

(def special-variable *table*)

(def (component e) table/abstract ()
  ())

(def component-environment table/abstract
  (bind ((*table* -self-))
    (call-next-method)))

(def (macro e) table ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'table/widget ,@args :rows (list ,@rows)))

;;;;;;
;;; Table widget

(def (component e) table/widget (table/abstract rows/mixin)
  ())

(def render-xhtml table/widget
  <table (:class "table")
    <tbody ,(render-rows-for -self-)>>)

(def render-csv table/widget
  (write-rows -self-))

(def render-ods table/widget
  <table:table ,(render-rows-for -self-)>)

;;;;;;
;;; Table header

(def (component e) table/header (table/widget row-headers/mixin column-headers/mixin)
  ())

(def render-xhtml table/header
  <table (:class "table")
    <thead <tr ,(render-column-headers -self-)>>
    <tbody ,(render-rows-for -self-)>>)

(def render-csv table/header
  (write-csv-line (column-headers-of -self-))
  (write-csv-line-separator)
  (render-rows-for -self-))

(def render-ods table/header
  <table:table
    <table:table-row ,(render-column-headers -self-)>
    ,(render-rows-for -self-)>)

;;;;;;
;;; Table widget

(def (component e) table/widget (table/header style/abstract page-navigation-bar/mixin)
  ())

(def render-xhtml table/widget
  (bind (((:read-only-slots rows page-navigation-bar id) -self-))
    (setf (total-count-of page-navigation-bar) (length rows))
    <div <table (:id ,id :class "table")
           <thead <tr ,(render-column-headers -self-)>>
           <tbody ,(bind ((visible-rows (subseq rows
                                                (position-of page-navigation-bar)
                                                (min (length rows)
                                                     (+ (position-of page-navigation-bar)
                                                        (page-size-of page-navigation-bar))))))
                         (iter (for row :in-sequence visible-rows)
                               (render-component row)))>>
         ,(when (< (page-size-of page-navigation-bar) (total-count-of page-navigation-bar))
            (render-component page-navigation-bar))>))
















(def (layered-function e) render-table-row-cells (table row column cell)
  (:method :before ((table table/mixin) (row row/widget) (column column-component) (cell cell/widget))
    (ensure-refreshed cell))

  (:method ((table table/mixin) (row row/widget) (column column-component) (cell cell/widget))
    (render-component cell))

  (:method :in xhtml-layer ((table table/mixin) (row row/widget) (column column-component) (cell component))
    <td ,(render-component cell)>)

  (:method :in xhtml-layer ((table table/mixin) (row row/widget) (column column-component) (cell string))
    <td ,(render-component cell)>)

  (:method :in ods-layer ((table table/mixin) (row row/widget) (column column-component) (cell component))
    <table:table-cell ,(render-component cell)>)

  (:method :in ods-layer ((table table/mixin) (row row/widget) (column column-component) (cell string))
    <table:table-cell ,(render-component cell)>))
