;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Table

(def special-variable *table*)

(def component table-component (remote-identity-component-mixin)
  ((columns nil :type components)
   (rows nil :type components)
   (page-navigation-bar (make-instance 'page-navigation-bar-component) :type component)))

(def component-environment table-component
  (bind ((*table* -self-))
    (call-next-method)))

(def render table-component ()
  (bind (((:read-only-slots rows page-navigation-bar id) -self-))
    (setf (total-count-of page-navigation-bar) (length rows))
    <div <table (:id ,id :class "table")
           <thead
            <tr ,(render-table-columns -self-)>>
           <tbody
            ,(bind ((visible-rows (subseq rows
                                          (position-of page-navigation-bar)
                                          (min (length rows)
                                               (+ (position-of page-navigation-bar)
                                                  (page-size-of page-navigation-bar))))))
                   (iter (for row :in-sequence visible-rows)
                         (render row)))>>
         ,(if (< (page-size-of page-navigation-bar) (total-count-of page-navigation-bar))
              (render page-navigation-bar)
              +void+)>))

(def render-csv table-component ()
  (render-csv-line (columns-of -self-))
  (render-csv-line-separator)
  (foreach #'render-csv (rows-of -self-)))

(def (layered-function e) render-table-columns (table-component)
  (:method ((self table-component))
    (foreach #'render (columns-of self))))

;;;;;;
;;; Column

(def component column-component (content-component)
  ((cell-factory nil :type (or null function))))

(def (macro e) column (content &rest args)
  `(make-instance 'column-component :content ,content ,@args))

(def generic pdf-column-width (column)
  (:method ((column column-component))
    ;; TODO: KLUDGE:
    100))

(def render column-component ()
  (bind ((id (generate-frame-unique-string)))
    <th <div (:id ,id) ,(call-next-method)>>
    `js(on-load (wui.setup-column ,id))))

;;;;;;
;;; Row

(def component row-component (remote-identity-component-mixin)
  ((cells nil :type components)))

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-method render-onclick-handler ((self row-component))
  nil)

(def (layered-function e) render-table-row (table row)
  (:method :around ((table table-component) (row row-component))
    (ensure-uptodate row)
    (if (force (visible-p row))
        (call-next-method)
        +void+))

  (:method ((table table-component) (row row-component))
    (bind (((:read-only-slots id) row)
           (table-id (id-of table))
           (onclick-handler? (render-onclick-handler row)))
      <tr (:id ,id :class ,(concatenate-string (table-row-style-class table row) (when onclick-handler? " selectable"))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,table-id ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,table-id ,id))
        ,(render-table-row-cells table row)>)))

(def (layered-function e) table-row-style-class (table row)
  (:method ((table table-component) (row row-component))
    (odd/even-class row (rows-of table))))

(def (layered-function e) render-table-row-cells (table row)
  (:method ((table table-component) (row row-component))
    (iter (for cell :in-sequence (cells-of row))
          (for column :in-sequence (columns-of table))
          (when (force (visible-p column))
            (render-table-cell table row column cell)))))

(def render row-component ()
  (render-table-row *table* -self-))

(def render-csv row-component ()
  (render-csv-line (cells-of -self-))
  (render-csv-line-separator))

;;;;;;
;;; Entire row

(def component entire-row-component (remote-identity-component-mixin content-component)
  ())

(def layered-method render-table-row ((table table-component) (row entire-row-component))
  (render row))

(def layered-method render-onclick-handler ((self entire-row-component))
  nil)

(def function render-entire-row (table row body-thunk)
  (bind (((:read-only-slots id) row)
         (table-id (id-of table))
         (onclick-handler? (render-onclick-handler row)))
    <tr (:id ,id :class ,(when onclick-handler? "selectable")
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,table-id ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,table-id ,id))
        <td (:colspan ,(length (columns-of table)))
            ,(funcall body-thunk)>>))

(def render entire-row-component ()
  (render-entire-row *table* -self- #'call-next-method))

;;;;;;
;;; Cell

(def component cell-component (content-component)
  ())

(def (layered-function e) render-table-cell (table row column cell)
  (:method :before ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((table table-component) (row row-component) (column column-component) (cell component))
    <td ,(render cell)>)

  (:method ((table table-component) (row row-component) (column column-component) (cell string))
    <td ,(render cell)>)
  
  (:method ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (render cell)))

(def render cell-component ()
  <td ,(call-next-method)>)
