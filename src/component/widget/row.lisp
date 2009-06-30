;;; Copyright (c) 2003-2009 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Row header abstract

(def (component e) row-header/abstract ()
  ((cell-factory :type (or null function))))

;;;;;;
;;; Row header widget

(def (component e) row-header/widget (row-header/abstract style/abstract content/mixin)
  ())

(def (macro e) row ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'row-header/widget ,@args :content ,(the-only-element content)))

(def render-xhtml row-header/abstract
  (with-render-style/abstract (-self- :element-name "th")
    (call-next-method)))

(def render-ods row-header/abstract
  <table:table-cell ,(call-next-method)>)

;;;;;;
;;; Row abstract

(def (component e) row/abstract ()
  ())

(def method supports-debug-component-hierarchy? ((self row/abstract))
  #f)

;;;;;;
;;; Row widget

(def (component e) row/widget (row/abstract style/abstract cells/mixin)
  ())

(def render-xhtml row/widget
  (render-table-row *table* -self-))

(def render-csv row/widget
  (write-csv-line (cells-of -self-))
  (write-csv-line-separator))

(def render-ods row/widget
  <table:table-row ,(bind ((table (parent-component-of -self-)))
                          (foreach (lambda (cell column)
                                     (render-cells table -self- column cell))
                                   (cells-of -self-)
                                   (columns-of table)))>)

(def function odd/even-class (component components)
  (if (zerop (mod (position component components) 2))
      "even-row"
      "odd-row"))

(def layered-method render-onclick-handler ((self row/widget) (button (eql :left)))
  nil)

(def (layered-function e) render-table-row (table row)
  (:method :around ((table table/mixin) (row row/widget))
    (ensure-refreshed row)
    (when (visible-component? row)
      (call-next-method)))

  (:method :in xhtml-layer ((table table/mixin) (row row/widget))
    (bind (((:read-only-slots id) row)
           (table-id (id-of table))
           (onclick-handler? (render-onclick-handler row :left)))
      <tr (:id ,id :class ,(concatenate-string (table-row-style-class table row) (when onclick-handler? " selectable"))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,table-id ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,table-id ,id))
        ,(render-table-row-cells table row)>)))

(def (layered-function e) table-row-style-class (table row)
  (:method ((table table/mixin) (row row/widget))
    (odd/even-class row (rows-of table))))

(def (layered-function e) render-table-row-cells (table row)
  (:method ((table table/mixin) (row row/widget))
    (iter (for cell :in-sequence (cells-of row))
          (for column :in-sequence (columns-of table))
          (when (visible-component? column)
            (render-cells table row column cell)))))

;;;;;;
;;; Entire row widget

(def (component e) entire-row/widget (row/abstract style/abstract content/mixin)
  ())

(def layered-method render-table-row ((table table/mixin) (row entire-row/widget))
  (render-component row))

(def layered-method render-onclick-handler ((self entire-row/widget) (button (eql :left)))
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

(def render-xhtml entire-row/widget
  (render-entire-row *table* -self- #'call-next-method))
