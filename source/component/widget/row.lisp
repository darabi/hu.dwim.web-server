;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; row/widget

(def (component e) row/widget (widget/style
                               row/layout
                               header/mixin
                               cells/mixin
                               context-menu/mixin
                               selectable/mixin)
  ())

(def (macro e) row/widget ((&rest args &key &allow-other-keys) &body cells)
  `(make-instance 'row/widget ,@args :cells (list ,@cells)))

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

(def layered-method render-onclick-handler ((self row/widget) (button (eql :left)))
  nil)

(def (layered-function e) render-table-row (table row)
  (:method :in xhtml-layer ((table table/widget) (row row/widget))
    (bind (((:read-only-slots id) row)
           (onclick-handler? (render-onclick-handler row :left)))
      <tr (:id ,id :class `str(,(table-row-style-class table row)
                               ,(when onclick-handler? " selectable")
                               ,(selectable-component-style-class row))
           :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
           :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
        ,(render-table-row-cells table row)>
      (render-context-menu-for row))))

(def (layered-function e) table-row-style-class (table row)
  (:method ((table table/widget) (row row/widget))
    (element-style-class *row-index* (length (rows-of table)))))

(def (layered-function e) render-table-row-cells (table row)
  (:method ((table table/widget) (row row/widget))
    (iter (for cell :in-sequence (cells-of row))
          (for column :in-sequence (columns-of table))
          (when (visible-component? column)
            (render-table-row-cell table row column cell)))))

;;;;;;
;;; entire-row/widget

(def (component e) entire-row/widget (widget/style
                                      row/abstract
                                      content/abstract
                                      header/mixin
                                      context-menu/mixin
                                      selectable/mixin)
  ())

(def (macro e) entire-row/widget ((&rest args &key &allow-other-keys) &body content)
  `(make-instance 'entire-row/widget ,@args :content ,(the-only-element content)))

(def layered-method render-table-row ((table table/widget) (row entire-row/widget))
  (render-component row))

(def layered-method render-onclick-handler ((self entire-row/widget) (button (eql :left)))
  nil)

(def render-xhtml entire-row/widget
  (bind (((:read-only-slots id) -self-)
         (onclick-handler? (render-onclick-handler -self- :left)))
    <tr (:id ,id :class ,(when onclick-handler? "selectable")
         :onmouseover `js-inline(wui.highlight-mouse-enter-handler event ,id)
         :onmouseout `js-inline(wui.highlight-mouse-leave-handler event ,id))
      <td (:colspan ,(length (columns-of *table*)))
        ,(render-content-for -self-)>>))
