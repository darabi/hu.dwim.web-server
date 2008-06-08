;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table

(def component table-component ()
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
       <table
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

(def component column-component (content-component)
  ((visible #t :type boolean)))

(def macro column (content)
  `(make-instance 'column-component :content ,content))

(def render column-component ()
  (when (visible-p -self-)
    <th ,(call-next-method)>))

(def component row-component ()
  ((cells nil :type components)))

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-function render-table-row (table row)
  (:method ((table table-component) (row row-component))
    (assert (eq (parent-component-of row) table))
    <tr (:class ,(odd/even-class row (rows-of table)))
      ,@(iter (for cell :in-sequence (cells-of row))
              (for column :in-sequence (columns-of table))
              (when (force (visible-p column))
                (collect (render-table-cell table row column cell))))>))

#+nil ; TODO delme?
(def render row-component ()
  (render-table-row (parent-component-of -self-) -self-))

(def component entire-row-component (content-component)
  ())

(def layered-method render-table-row ((table table-component) (row entire-row-component))
  (render row))

(def function render-entire-row (table body-thunk)
  <tr
   <td (:colspan ,(length (columns-of table)))
       ,(funcall body-thunk)>>)

(def render entire-row-component ()
  (render-entire-row (parent-component-of -self-) #'call-next-method))

(def component cell-component (content-component)
  ())

(def layered-function render-table-cell (table row column cell)
  (:method (table row column cell)
    <td ,(render cell)>))

#+nil ; TODO delme?
(def render cell-component ()
  <td ,(call-next-method)>)
