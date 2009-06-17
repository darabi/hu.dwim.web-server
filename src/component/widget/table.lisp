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

(def (component ea) table/abstract ()
  ())

(def component-environment table/abstract
  (bind ((*table* -self-))
    (call-next-method)))

(def (macro e) table ((&rest args &key &allow-other-keys) &body rows)
  `(make-instance 'row-based-table/full ,@args :rows (list ,@rows)))

;;;;;;
;;; Row based table basic

(def (component ea) row-based-table/basic (table/abstract rows/mixin)
  ())

(def render-xhtml row-based-table/basic
  <table (:class "table")
    <tbody ,(render-rows -self-)>>)

(def render-csv row-based-table/basic
  (write-rows -self-))

(def render-ods row-based-table/basic
  <table:table ,(render-rows -self-)>)

;;;;;;
;;; Row based table basic

;; TODO:
(def (component ea) column-based-table/basic (table/abstract columns/mixin)
  ())

;;;;;;
;;; Row based table header

(def (component ea) row-based-table/header (row-based-table/basic row-headers/mixin column-headers/mixin)
  ())

(def render-xhtml row-based-table/header
  <table (:class "table")
    <thead <tr ,(render-column-headers -self-)>>
    <tbody ,(render-rows -self-)>>)

(def render-csv row-based-table/header
  (write-csv-line (column-headers-of -self-))
  (write-csv-line-separator)
  (render-rows -self-))

(def render-ods row-based-table/header
  <table:table
    <table:table-row ,(render-column-headers -self-)>
    ,(render-rows -self-)>)

;;;;;;
;;; Row based table full

(def (component ea) row-based-table/full (row-based-table/header style/abstract page-navigation-bar/mixin)
  ())

(def render-xhtml row-based-table/full
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
