;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table

(def component table-component (remote-identity-component-mixin)
  ((columns nil :type components)
   (rows nil :type components)
   (page-navigation-bar nil :type component)))

(def constructor table-component ()
  (with-slots (page-navigation-bar) -self-
    (setf page-navigation-bar (make-instance 'page-navigation-bar-component :position 0 :page-count 10))))

(def render table-component ()
  (bind ((table -self-))
    (with-slots (columns rows page-navigation-bar) table
      (setf (total-count-of page-navigation-bar) (length rows))
      <div
       <table (:class "table")
           <thead
            <tr ,@(mapcar #'render columns)>>
         <tbody
          ,@(bind ((visible-rows (subseq rows
                                         (position-of page-navigation-bar)
                                         (min (length rows)
                                              (+ (position-of page-navigation-bar)
                                                 (page-count-of page-navigation-bar))))))
              (iter (for row :in-sequence visible-rows)
                    (collect (render-table-row table row))))>>
       ,(if (< (page-count-of page-navigation-bar) (total-count-of page-navigation-bar))
            (render page-navigation-bar)
            +void+)>)))

;;;;;;
;;; Column

(def component column-component (content-component)
  ((cell-factory nil :type (or null function))))

(def macro column (content &rest args)
  `(make-instance 'column-component :content ,content ,@args))

(def render column-component ()
  <th ,(call-next-method)>)

;;;;;;
;;; Row

(def component row-component (remote-identity-component-mixin)
  ((cells nil :type components)))

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-function render-table-row (table row)
  (:method :around (table row)
    (ensure-uptodate row)
    (if (force (visible-p row))
        (call-next-method)
        +void+))

  (:method ((table table-component) (row row-component))
    (assert (eq (parent-component-of row) table))
    <tr (:class ,(odd/even-class row (rows-of table)))
      ,@(iter (for cell :in-sequence (cells-of row))
              (for column :in-sequence (columns-of table))
              (when (force (visible-p column))
                (collect (render-table-cell table row column cell))))>))

(def render row-component ()
  (render-table-row (parent-component-of -self-) -self-))

;;;;;;
;;; Entire row

(def component entire-row-component (remote-identity-component-mixin content-component)
  ())

(def layered-method render-table-row ((table table-component) (row entire-row-component))
  (render row))

(def function render-entire-row (table body-thunk)
  <tr <td (:colspan ,(length (columns-of table)))
          ,(funcall body-thunk)>>)

(def render entire-row-component ()
  (render-entire-row (parent-component-of -self-) #'call-next-method))

;;;;;;
;;; Cell

(def component cell-component (content-component)
  ())

(def layered-function render-table-cell (table row column cell)
  (:method :before (table row column cell)
    (ensure-uptodate cell))
  
  (:method (table row column cell)
    <td ,(render (content-of cell)) >))

(def render cell-component ()
  <td ,(call-next-method)>)
