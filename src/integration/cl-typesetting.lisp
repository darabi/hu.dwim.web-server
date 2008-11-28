;;; Copyright (c) 2003-2008 by the authors.
;;;
;;; See LICENCE and AUTHORS for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Render

(def layered-method execute-export-pdf ((component component))
  (bind ((typeset::*default-font* (pdf:get-font "Times-Roman"))
         (typeset::*font* typeset::*default-font*))
    (typeset::with-document ()
      (typeset:draw-pages (typeset:compile-text ()
                            (render-pdf component))
                          :margins '(72 72 72 50))
      (when pdf:*page*
        (typeset:finalize-page pdf:*page*))
      (typeset:write-document *pdf-stream*))))

(def render-pdf string ()
  (typeset:put-string -self-))

(def render-pdf command-bar-component ()
  (iter (for command :in (commands-of -self-))
        (unless (first-iteration-p)
          (typeset:put-string " "))
        (render-pdf command)))

(def render-pdf primitive-component ()
  (typeset:put-string (print-component-value -self-)))

(def render-pdf standard-object-detail-component ()
  (typeset:table (:col-widths '(100 200) :border 1)
    (foreach #'render-pdf (slot-value-groups-of -self-))))

(def render-pdf standard-object-slot-value-group-component ()
  (foreach (lambda (slot-value)
             (typeset:row ()
               (render-pdf slot-value)))
           (slot-values-of -self-)))

(def render-pdf standard-object-slot-value-component ()
  (typeset:cell ()
    (render-pdf (label-of -self-)))
  (typeset:cell ()
    (render-pdf (value-of -self-))))

(def render-pdf table-component ()
  (typeset:table (:col-widths (mapcar 'pdf-column-width (columns-of -self-)))
    (typeset:row ()
      (foreach #'render-pdf (columns-of -self-)))
    (foreach #'render-pdf (root-nodes-of -self-))))

(def render-pdf column-component ()
  (typeset:cell ()
    (call-next-method)))

(def render-pdf cell-component ()
  (typeset:cell ()
    (call-next-method)))

(def render-pdf row-component ()
  (typeset:row ()
    (bind ((table (parent-component-of -self-)))
      (foreach (lambda (cell column)
                 (render-pdf-table-cell table -self- column cell))
               (cells-of -self-)
               (columns-of table)))))

(def (layered-function e) render-pdf-table-cell (table row column cell)
  (:method :before ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((table table-component) (row row-component) (column column-component) (cell component))
    (typeset:cell ()
      (render-pdf cell)))

  (:method ((table table-component) (row row-component) (column column-component) (cell string))
    (typeset:cell ()
      (render-pdf cell)))
  
  (:method ((table table-component) (row row-component) (column column-component) (cell cell-component))
    (render-pdf cell)))

(def render-pdf tree-component ()
  (typeset:table (:col-widths (mapcar 'pdf-column-width (columns-of -self-)))
    (typeset:row ()
      (foreach #'render-pdf (columns-of -self-)))
    (foreach #'render-pdf (root-nodes-of -self-))))

(def render-pdf node-component ()
  (typeset:row ()
    (bind ((tree (find-ancestor-component-with-type -self- 'tree-component)))
      (foreach (lambda (cell column)
                 (render-pdf-tree-cell tree -self- column cell))
               (cells-of -self-)
               (columns-of tree))))
  (foreach #'render-pdf (child-nodes-of -self-)))

(def (layered-function e) render-pdf-tree-cell (tree node column cell)
  (:method :before ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (ensure-uptodate cell))

  (:method ((tree tree-component) (node node-component) (column column-component) (cell component))
    (typeset:cell ()
      (render-pdf cell)))

  (:method ((tree tree-component) (node node-component) (column column-component) (cell string))
    (typeset:cell ()
      (render-pdf cell)))
  
  (:method ((tree tree-component) (node node-component) (column column-component) (cell cell-component))
    (render-pdf cell)))
