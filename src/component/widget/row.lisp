;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Rows mixin

(def (component ea) rows/mixin ()
  ((rows :type components)))

(def layered-function render-rows (component)
  (:method ((self rows/mixin))
    (foreach #'render-component (rows-of self))))

;;;;;;
;;; Row headers mixin

(def (component ea) row-headers/mixin ()
  ((row-headers :type components)))

(def layered-function render-row-headers (component)
  (:method ((self row-headers/mixin))
    (foreach #'render-component (row-headers-of self))))

;;;;;;
;;; Row header abstract

(def (component ea) row-header/abstract ()
  ((cell-factory :type (or null function))))

;;;;;;
;;; Row header basic

(def (component ea) row-header/basic (row-header/abstract style/abstract content/mixin)
  ())

(def (macro e) row ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'row-header/basic ,@args :content ,(the-only-element content)))

(def render-xhtml row-header/abstract
  (with-render-style/abstract (-self- :element-name "th")
    (call-next-method)))

(def render-ods row-header/abstract
  <table:table-cell ,(call-next-method)>)

;;;;;;
;;; Row abstract

(def (component ea) row/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self row/abstract))
  #f)

;;;;;;
;;; Row basic

(def (component ea) row/basic (row/abstract style/abstract cells/mixin)
  ())

(def render-xhtml row/basic
  (render-table-row *table* -self-))

(def render-csv row/basic
  (write-csv-line (cells-of -self-))
  (write-csv-line-separator))

(def render-ods row/basic
  <table:table-row ,(bind ((table (parent-component-of -self-)))
                          (foreach (lambda (cell column)
                                     (render-cells table -self- column cell))
                                   (cells-of -self-)
                                   (columns-of table)))>)

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-method render-onclick-handler ((self row/basic) (button (eql :left)))
  nil)

(def (layered-function e) render-table-row (table row)
  (:method :around ((table table/mixin) (row row/basic))
    (ensure-refreshed row)
    (when (visible? row)
      (call-next-method)))

  (:method :in xhtml-layer ((table table/mixin) (row row/basic))
    (bind (((:read-only-slots id) row)
           (table-id (id-of table))
           (onclick-handler? (render-onclick-handler row :left)))
      <tr (:id ,id :class ,(concatenate-string (table-row-style-class table row) (when onclick-handler? " selectable"))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,table-id ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,table-id ,id))
        ,(render-table-row-cells table row)>)))

(def (layered-function e) table-row-style-class (table row)
  (:method ((table table/mixin) (row row/basic))
    (odd/even-class row (rows-of table))))

(def (layered-function e) render-table-row-cells (table row)
  (:method ((table table/mixin) (row row/basic))
    (iter (for cell :in-sequence (cells-of row))
          (for column :in-sequence (columns-of table))
          (when (visible? column)
            (render-cells table row column cell)))))

;;;;;;
;;; Entire row basic

(def (component ea) entire-row/basic (row/abstract style/abstract content/mixin)
  ())

(def layered-method render-table-row ((table table/mixin) (row entire-row/basic))
  (render-component row))

(def layered-method render-onclick-handler ((self entire-row/basic) (button (eql :left)))
  nil)

(def function render-entire-row (table row body-thunk)
  (bind (((:read-only-slots id) row)
         (table-id (id-of table))
         (onclick-handler? (render-onclick-handler row :left)))
    <tr (:id ,id :class ,(when onclick-handler? "selectable")
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,table-id ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,table-id ,id))
        <td (:colspan ,(length (columns-of table)))
            ,(funcall body-thunk)>>))

(def render-xhtml entire-row/basic
  (render-entire-row *table* -self- #'call-next-method))
