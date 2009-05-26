;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Constants

;; TODO: what about other constants such as left, middle? find better names and values
(def (constant e :test 'string=) +table-cell-horizontal-align-css-class/right+ "_hra")
(def (constant e :test 'string=) +table-cell-horizontal-align-css-class/center+ "_hca")

(def (constant e :test 'string=) +table-cell-vertical-align-css-class/top+ "_vta")
(def (constant e :test 'string=) +table-cell-vertical-align-css-class/bottom+ "_vba")

(def (constant e :test 'string=) +table-cell-nowrap-css-class+ "_nw")

;;;;;;
;;; Table component

(def special-variable *table*)

(def component table-component (remote-identity-mixin)
  ((columns nil :type components)
   (rows nil :type components)
   (page-navigation-bar (make-instance 'page-navigation-bar-component) :type component)))

(def component-environment table-component
  (bind ((*table* -self-))
    (call-next-method)))

(def render-xhtml table-component
  (bind (((:read-only-slots rows page-navigation-bar id) -self-))
    (setf (total-count-of page-navigation-bar) (length rows))
    <div <table (:id ,id :class "table")
           <thead <tr ,(render-table-columns -self-)>>
           <tbody ,(bind ((visible-rows (subseq rows
                                                (position-of page-navigation-bar)
                                                (min (length rows)
                                                     (+ (position-of page-navigation-bar)
                                                        (page-size-of page-navigation-bar))))))
                         (iter (for row :in-sequence visible-rows)
                               (render row)))>>
         ,(when (< (page-size-of page-navigation-bar) (total-count-of page-navigation-bar))
            (render page-navigation-bar))>))

(def render-csv table-component
  (render-csv-line (columns-of -self-))
  (render-csv-line-separator)
  (foreach #'render (rows-of -self-)))

(def render-ods table-component
  <table:table
    <table:table-row ,(foreach #'render (columns-of -self-))>
    ,(foreach #'render (rows-of -self-))>)

(def (layered-function e) render-table-columns (table-component)
  (:method ((self table-component))
    (foreach #'render (columns-of self))))

;;;;;;
;;; Column component

(def component column-component (content-mixin remote-setup-mixin)
  ((cell-factory nil :type (or null function))))

(def (macro e) column (content &rest args)
  `(make-instance 'column-component :content ,content ,@args))

(def render-xhtml column-component
  <th <div (:id ,(id-of -self-)) ,(call-next-method)>>)

(def render-ods column-component
  <table:table-cell ,(call-next-method)>)

;;;;;;
;;; Row component

(def component row-component (remote-identity-mixin)
  ((cells nil :type components)))

(def render-xhtml row-component
  (render-table-row *table* -self-))

(def render-csv row-component
  (render-csv-line (cells-of -self-))
  (render-csv-line-separator))

(def render-ods row-component
  <table:table-row ,(bind ((table (parent-component-of -self-)))
                          (foreach (lambda (cell column)
                                     (render-table-cell table -self- column cell))
                                   (cells-of -self-)
                                   (columns-of table)))>)

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-method render-onclick-handler ((self row-component))
  nil)

(def (layered-function e) render-table-row (table row)
  (:method :around ((table table-component) (row row-component))
    (ensure-uptodate row)
    (when (force (visible-p row))
      (call-next-method)))

  (:method :in xhtml-format ((table table-component) (row row-component))
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

;;;;;;
;;; Entire row component

(def component entire-row-component (remote-identity-mixin content-mixin)
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

(def render-xhtml entire-row-component
  (render-entire-row *table* -self- #'call-next-method))

;;;;;;
;;; Cell component

(def component cell-component (content-mixin)
  ((column-span nil)
   (row-span nil)
   (word-wrap :type boolean :accessor word-wrap?)
   (horizontal-alignment nil :type (member nil :left :right :center))
   (vertical-alignment nil :type (member nil :top :bottom :center))))

(def with-macro* render-cell-component (cell &key css-class)
  (setf css-class (ensure-list css-class))
  (if (typep cell 'cell-component)
      (bind (((:read-only-slots column-span row-span horizontal-alignment vertical-alignment) cell))
        (ecase horizontal-alignment
          (:right (push +table-cell-horizontal-align-css-class/right+ css-class))
          (:center (push +table-cell-horizontal-align-css-class/center+ css-class))
          ((:left nil) nil))
        (ecase vertical-alignment
          (:top (push +table-cell-vertical-align-css-class/top+ css-class))
          (:bottom (push +table-cell-vertical-align-css-class/bottom+ css-class))
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

(def render-xhtml cell-component
  (render-cell-component (-self-)
    (call-next-method)))

(def render-ods cell-component
  <table:table-cell ,(call-next-method)>)

(def (layered-function e) render-table-cell (table row column cell)
  (:method :before ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (render cell))

  (:method :in xhtml-format ((table table-component) (row row-component) (column column-component) (cell component))
    <td ,(render cell)>)

  (:method :in xhtml-format ((table table-component) (row row-component) (column column-component) (cell string))
    <td ,(render cell)>)

  (:method :in ods-format ((table table-component) (row row-component) (column column-component) (cell component))
    <table:table-cell ,(render cell)>)

  (:method :in ods-format ((table table-component) (row row-component) (column column-component) (cell string))
    <table:table-cell ,(render cell)>))
